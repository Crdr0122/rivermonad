{-# LANGUAGE RecordWildCards #-}

module Layout where

import Config
import Control.Arrow ((&&&))
import Control.Concurrent.MVar
import Control.Monad (unless, when)
import Control.Monad.State hiding (state)
import Data.Bimap qualified as B
import Data.Bits
import Data.Foldable
import Data.List
import Data.Map.Strict qualified as M
import Data.Sequence qualified as S
import Data.Word
import Optics.Core
import Optics.State
import Optics.State.Operators
import Protocols.Generated
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.Connection

startLayout :: MVar WMState -> W ()
startLayout stateMVar = do
  modifyMVarW_ stateMVar $ \state -> do
    let newState = execState sortNewWindows state
    newState ^. #manageQueue
    let
      o = newState ^. #focusedOut
      seat = (newState ^. #focusedSeat)

    maybe (riverSeatV1ClearFocus seat) (riverSeatV1FocusWindow seat) (newState ^. #focusedWin)

    if o /= nonObject
      then case newState ^? #allOutputs % at o %? #outLayerShellObj of
        Nothing -> pure ()
        Just oRec -> riverLayerShellOutputV1SetDefault oRec
      else pure ()

    pure $ newState & #manageQueue .~ pure ()
  state <- liftIO $ readMVar stateMVar
  mapM_ (startLayoutOutput stateMVar) $ B.toList (state ^. #allOutputWorkspaces)
 where
  sortNewWindows = do
    queue <- use #newWindowQueue
    #newWindowQueue .= []
    workmaps <- use #allOutputWorkspaces
    focusedWS <- use (focusedWorkspace % non 1)
    forM_ queue $ \winPtr -> do
      use (#allWindows % at winPtr) >>= \case
        Nothing -> pure ()
        Just win -> do
          let (targetWS, status) = (getWorkspace, getStatus)
              getWorkspace =
                findOf
                  folded
                  ( \(t, a, _) ->
                      t `isInfixOf` (win ^. #winTitle)
                        && a `isInfixOf` (win ^. #winAppId)
                  )
                  (myConfig ^. #workspaceRules)
                  ^. non ("", "", focusedWS)
                  % _3

              getStatus =
                findOf
                  folded
                  ( \(t, a, _) ->
                      t `isInfixOf` (win ^. #winTitle)
                        && a `isInfixOf` (win ^. #winAppId)
                  )
                  (myConfig ^. #floatingRules)
                  ^. non ("", "", Tiled)
                  % _3

          case status of
            Tiled -> #allWorkspacesTiled %= BS.insert targetWS winPtr
            Floating -> #floatingQueue % at targetWS %?= (winPtr :)
            Fullscreen -> #fullscreenQueue % at targetWS %?= (winPtr :)
            FullscreenFloating -> #fullscreenQueue % at targetWS %?= (winPtr :)

          let getSize =
                findOf
                  folded
                  ( \(t, a, _, _) ->
                      t `isInfixOf` (win ^. #winTitle)
                        && a `isInfixOf` (win ^. #winAppId)
                  )
                  (myConfig ^. #windowSizeRules)
                  ^. non ("", "", 0, 0)
                  % to ((^. _3) &&& (^. _4))

          case getSize of
            (0, 0) -> pure ()
            size -> #allWindows % at winPtr %? #winSizeRule ?= size

          when (targetWS == focusedWS) $ setFocusedWindowAndHistory focusedWS winPtr
          unless (targetWS `elem` B.keysR workmaps) $ #renderQueue >>>= riverWindowV1Hide winPtr

startLayoutOutput :: MVar WMState -> (Object RiverOutputV1, WorkspaceID) -> W ()
startLayoutOutput stateMVar (output, ws) = modifyMVarW_ stateMVar $ \state ->
  case state ^? #allOutputs % at output %? #outGeo of
    Nothing -> pure state
    Just geom -> do
      let newState = execState (layoutEngine geom) state
      view #manageQueue newState
      pure $ newState & #manageQueue .~ pure ()
 where
  raiseAllWindows = mapM_ (riverNodeV1PlaceTop . winNodeObj)
  shrinkWindows b = fmap (& _2 %~ \r -> r & #rx %~ (+ b) & #ry %~ (+ b) & #rh %~ subtract (2 * b) & #rw %~ subtract (2 * b))
  layoutEngine geom =
    use (#workspaceLayouts % at ws) >>= \case
      Nothing -> pure ()
      Just currentLayout -> do
        allWindows <- use #allWindows
        tilingPtrs <- use (#allWorkspacesTiled % to (BS.lookupBs ws))
        fWin <- use #focusedWin
        -- Tiled
        let idx = fWin >>= (`S.elemIndexL` tilingPtrs)
            tileable = (allWindows M.!) <$> tilingPtrs
            rawTiles = applySomeLayout currentLayout idx geom tileable
            bordered = shrinkWindows (myConfig ^. #borderPx) $ shrinkWindows (myConfig ^. #gapPx) (toList rawTiles)

        forM_ bordered $ \(win, rect@Rect{..}) -> do
          let ptr = win ^. #winObj
              node = win ^. #winNodeObj
          #allWindows % at ptr %? #winTileGeo ?= rect
          #manageQueue >>>= riverWindowV1ProposeDimensions ptr rw rh
          #renderQueue >>>= (riverNodeV1SetPosition node rx ry >> riverWindowV1SetContentClipBox ptr 0 0 rw rh >> riverNodeV1PlaceBottom node)

        -- Floating
        queuedFloatingPtrs <- use (#floatingQueue % at ws % non [])
        alreadyFloating <- use (#allWorkspacesFloating % to (BS.lookupBs ws) % to S.length)
        let newFloatingWindows = (allWindows M.!) <$> queuedFloatingPtrs
            (floatingPositions, floatMAction, floatRAction) =
              calculateFloatingPositions geom newFloatingWindows alreadyFloating
        #allWorkspacesFloating %= BS.insertList ws queuedFloatingPtrs
        #manageQueue >>>= floatMAction
        #renderQueue >>>= floatRAction
        forM_ (zip newFloatingWindows floatingPositions) $ \(win, rect) -> do
          let ptr = win ^. #winObj
          #allWindows % at ptr %?= \w -> w & #winFloatGeo ?~ rect & #winFloat .~ True
          #renderQueue >>>= riverWindowV1SetContentClipBox ptr 0 0 0 0

        -- Fullscreen
        newFullscreenPtrs <- use (#fullscreenQueue % at ws % non [])
        let newFullscreenWindows = (allWindows M.!) <$> newFullscreenPtrs
        #allWorkspacesFullscreen %= BS.insertList ws newFullscreenPtrs
        forM_ newFullscreenPtrs $ \ptr -> do
          #allWindows % at ptr %? #winFull .= True
          #manageQueue >>>= (riverWindowV1Fullscreen ptr output >> riverWindowV1InformFullscreen ptr)
        #renderQueue >>>= raiseAllWindows (reverse newFullscreenWindows)

        -- Borders
        floatingPtrs <- use (#allWorkspacesFloating % to (BS.lookupBs ws))
        #manageQueue >>>= mapM_ (renderBorder fWin bColor fColor pColor (myConfig ^. #borderPx)) tileable
        #manageQueue >>>= mapM_ (renderBorder fWin bColor fColor pColor (myConfig ^. #borderPx)) ((allWindows M.!) <$> floatingPtrs)

        -- Cleanup Queues
        #floatingQueue % at ws ?= []
        #fullscreenQueue % at ws ?= []

renderBorder :: Maybe (Object RiverWindowV1) -> (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32) -> (Word32, Word32, Word32, Word32) -> Int32 -> Window -> W ()
renderBorder Nothing (r, g, b, a) _ _ bPx w = riverWindowV1SetBorders (winObj w) allEdges bPx r g b a
renderBorder (Just focused) (r, g, b, a) (fr, fg, fb, fa) (pr, pg, pb, pa) bPx Window{winObj, winPinned}
  | winPinned = riverWindowV1SetBorders winObj allEdges bPx pr pg pb pa
  | winObj == focused = riverWindowV1SetBorders winObj allEdges bPx fr fg fb fa
  | otherwise = riverWindowV1SetBorders winObj allEdges bPx r g b a

translateColor :: Word32 -> (Word32, Word32, Word32, Word32)
translateColor rgba = (r', g', b', a')
 where
  -- Extract 8-bit components
  r = (rgba `shiftR` 24) .&. 0xFF
  g = (rgba `shiftR` 16) .&. 0xFF
  b = (rgba `shiftR` 8) .&. 0xFF
  a = rgba .&. 0xFF

  -- 1. Scale to 32-bit range (0xFF -> 0xFFFFFFFF)
  -- We use (x * 0x01010101) to distribute the 8 bits across 32 bits evenly
  scale8to32 :: Word32 -> Word64
  scale8to32 x = fromIntegral x * 0x01010101 :: Word64

  a32 = scale8to32 a

  -- 2. Pre-multiply (Color * Alpha / MaxAlpha)
  -- We use Word64 to prevent overflow during multiplication
  premult c = (scale8to32 c * a32) `div` 0xFFFFFFFF

  r' = fromIntegral (premult r) :: Word32
  g' = fromIntegral (premult g) :: Word32
  b' = fromIntegral (premult b) :: Word32
  a' = fromIntegral a32 :: Word32

bColor, fColor, pColor :: (Word32, Word32, Word32, Word32)
bColor = translateColor (borderColor myConfig)
fColor = translateColor (focusedBorderColor myConfig)
pColor = translateColor (pinnedBorderColor myConfig)
