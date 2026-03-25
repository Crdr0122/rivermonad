{-# LANGUAGE RecordWildCards #-}

module Layout (startLayout) where

import Config
import Control.Concurrent.MVar
import Control.Monad (unless, when)
import Control.Monad.State hiding (state)
import Data.Bimap qualified as B
import Data.Foldable
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as S
import Foreign
import Foreign.C
import Optics.Core
import Optics.State
import Optics.State.Operators
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.ImportedFunctions

startLayout :: MVar WMState -> IO ()
startLayout stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    let newState = execState sortNewWindows state
    view #manageQueue newState
    pure $ newState & #manageQueue .~ pure ()
  state <- readMVar stateMVar
  let outputs = B.toList (state ^. #allOutputWorkspaces)
  mapM_ (\(o, w) -> startLayoutOutput o w stateMVar) outputs
 where
  sortNewWindows = do
    queue <- use #newWindowQueue
    #newWindowQueue .= []
    fOutput <- use #focusedOutput
    workmaps <- use #allOutputWorkspaces
    seat <- use #focusedSeat
    let focusedWS = fromMaybe 1 $ B.lookup fOutput workmaps
    forM_ queue $ \winPtr -> do
      mWin <- preuse (#allWindows % at winPtr % _Just)
      case mWin of
        Nothing -> pure ()
        Just win -> do
          let (targetWS, status) = (getWorkspace, getStatus)
              getWorkspace =
                findOf folded (\(t, a, _) -> t `isInfixOf` (win ^. #winTitle) && a `isInfixOf` (win ^. #winAppID)) (myConfig ^. #workspaceRules) ^. non ("", "", focusedWS) % _3
              getStatus =
                findOf folded (\(t, a, _) -> t `isInfixOf` (win ^. #winTitle) && a `isInfixOf` (win ^. #winAppID)) (myConfig ^. #floatingRules) ^. non ("", "", Tiled) % _3
          case status of
            Tiled -> #allWorkspacesTiled %= BS.insert targetWS winPtr
            Floating -> #floatingQueue % at targetWS %?= (winPtr :)
            Fullscreen -> #fullscreenQueue % at targetWS %?= (winPtr :)
            FullscreenFloating -> #fullscreenQueue % at targetWS %?= (winPtr :)

          when (targetWS == focusedWS) $ do
            #focusedWindow ?= winPtr
            #workspaceFocusHistory % at focusedWS ?= winPtr
            #manageQueue <>= riverSeatFocusWindow seat winPtr
          unless (targetWS `elem` B.keysR workmaps) $ #renderQueue <>= riverWindowHide winPtr

startLayoutOutput :: Ptr RiverOutput -> WorkspaceID -> MVar WMState -> IO ()
startLayoutOutput output ws stateMVar = modifyMVar_ stateMVar $ \(state :: WMState) ->
  case state ^? #allOutputs % at output %? #outGeometry of
    Nothing -> pure state
    Just geom -> do
      let newState = execState (layoutEngine geom) state
      view #manageQueue newState
      pure $ newState & #manageQueue .~ pure ()
 where
  raiseAllWindows = mapM_ (riverNodePlaceTop . nodePtr)
  shrinkWindows b = fmap (& _2 %~ \r -> r & #rx %~ (+ b) & #ry %~ (+ b) & #rh %~ subtract (2 * b) & #rw %~ subtract (2 * b))
  layoutEngine geom = do
    allWindows <- use #allWindows
    tilingPtrs <- use (#allWorkspacesTiled % to (BS.lookupBs ws))
    fWin <- use #focusedWindow
    mCurrentLayout <- preuse (#workspaceLayouts % at ws % _Just)
    case mCurrentLayout of
      Nothing -> pure ()
      Just currentLayout -> do
        -- Tiled
        let idx = fWin >>= (`S.elemIndexL` tilingPtrs)
            tileable = (allWindows M.!) <$> tilingPtrs
            rawTiles = applySomeLayout currentLayout idx geom tileable
            bordered = shrinkWindows (myConfig ^. #borderPx) $ shrinkWindows (myConfig ^. #gapPx) (toList rawTiles)

        forM_ bordered $ \(win, rect@Rect{..}) -> do
          let ptr = win ^. #winPtr
              node = win ^. #nodePtr
          #allWindows % at ptr %? #tilingGeometry ?= rect
          #manageQueue <>= riverWindowProposeDimensions ptr rw rh
          #renderQueue <>= (riverNodeSetPosition node rx ry >> riverWindowSetContentClipBox ptr 0 0 rw rh >> riverNodePlaceBottom node)

        -- Floating
        queuedFloatingPtrs <- use (#floatingQueue % at ws % non [])
        let newFloatingWindows = (allWindows M.!) <$> queuedFloatingPtrs
            (floatingPositions, floatMAction, floatRAction) = calculateFloatingPositions geom newFloatingWindows
        #allWorkspacesFloating %= BS.insertList ws queuedFloatingPtrs
        #manageQueue <>= floatMAction
        #renderQueue <>= floatRAction
        forM_ (zip newFloatingWindows floatingPositions) $ \(win, rect) -> do
          let ptr = win ^. #winPtr
          #allWindows % at ptr % _Just %= \w -> w & #floatingGeometry ?~ rect & #isFloating .~ True
          #renderQueue <>= riverWindowSetContentClipBox ptr 0 0 0 0

        -- Fullscreen
        newFullscreenPtrs <- use (#fullscreenQueue % at ws % non [])
        let newFullscreenWindows = (allWindows M.!) <$> newFullscreenPtrs
        #allWorkspacesFullscreen %= BS.insertList ws newFullscreenPtrs
        forM_ newFullscreenPtrs $ \ptr -> do
          #allWindows % at ptr %? #isFullscreen .= True
          #manageQueue <>= (riverWindowFullscreen ptr output >> riverWindowInformFullscreen ptr)
        #renderQueue <>= raiseAllWindows (reverse newFullscreenWindows)

        -- Borders
        floatingPtrs <- use (#allWorkspacesFloating % to (BS.lookupBs ws))
        #manageQueue <>= mapM_ (renderBorder fWin bColor fColor pColor (myConfig ^. #borderPx)) tileable
        #manageQueue <>= mapM_ (renderBorder fWin bColor fColor pColor (myConfig ^. #borderPx)) ((allWindows M.!) <$> floatingPtrs)

        -- Cleanup Queues
        #floatingQueue % at ws ?= []
        #fullscreenQueue % at ws ?= []

renderBorder :: Maybe (Ptr RiverWindow) -> (CUInt, CUInt, CUInt, CUInt) -> (CUInt, CUInt, CUInt, CUInt) -> (CUInt, CUInt, CUInt, CUInt) -> CInt -> Window -> IO ()
renderBorder Nothing (r, g, b, a) _ _ bPx w = riverWindowSetBorders (winPtr w) edgeAll bPx r g b a
renderBorder (Just focused) (r, g, b, a) (fr, fg, fb, fa) (pr, pg, pb, pa) bPx Window{winPtr, isPinned}
  | isPinned = riverWindowSetBorders winPtr edgeAll bPx pr pg pb pa
  | winPtr == focused = riverWindowSetBorders winPtr edgeAll bPx fr fg fb fa
  | otherwise = riverWindowSetBorders winPtr edgeAll bPx r g b a

translateColor :: Word32 -> (CUInt, CUInt, CUInt, CUInt)
translateColor rgba = (toCU r', toCU g', toCU b', toCU a')
 where
  -- Extract 8-bit components
  r = (rgba `shiftR` 24) .&. 0xFF
  g = (rgba `shiftR` 16) .&. 0xFF
  b = (rgba `shiftR` 8) .&. 0xFF
  a = rgba .&. 0xFF

  -- 1. Scale to 32-bit range (0xFF -> 0xFFFFFFFF)
  -- We use (x * 0x01010101) to distribute the 8 bits across 32 bits evenly
  scale8to32 x = fromIntegral x * 0x01010101 :: Word64

  a32 = scale8to32 a

  -- 2. Pre-multiply (Color * Alpha / MaxAlpha)
  -- We use Word64 to prevent overflow during multiplication
  premult c = (scale8to32 c * a32) `div` 0xFFFFFFFF

  r' = fromIntegral (premult r) :: Word32
  g' = fromIntegral (premult g) :: Word32
  b' = fromIntegral (premult b) :: Word32
  a' = fromIntegral a32 :: Word32

  toCU = fromIntegral

bColor, fColor, pColor :: (CUInt, CUInt, CUInt, CUInt)
bColor = translateColor (borderColor myConfig)
fColor = translateColor (focusedBorderColor myConfig)
pColor = translateColor (pinnedBorderColor myConfig)
