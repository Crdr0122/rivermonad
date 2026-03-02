module Layout (startLayout) where

import Config
import Data.Foldable
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Sequence qualified as S
import Foreign
import Foreign.C
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.ImportedFunctions

startLayout :: IORef WMState -> IO ()
startLayout stateIORef = do
  state@WMState
    { focusedOutput
    , allOutputs
    , workspaceRatios
    , floatingQueue
    , focusedWorkspace
    , workspaceLayouts
    , allWorkspacesFloating
    , allWorkspacesTiled
    , allWorkspacesFullscreen
    , allWindows
    , fullscreenQueue
    } <-
    readIORef stateIORef
  let mOut = M.lookup focusedOutput allOutputs
  case mOut of
    Nothing -> pure ()
    Just o@Output{outX, outY, outHeight, outWidth} -> do
      let (tileable, floatingWindowsQueue, fullscreenedWindowsQueue, nonFullscreenPtrs) = getWindows
          geometry = Rect{rx = outX, ry = outY, rh = outHeight, rw = outWidth}
          ratio = workspaceRatios M.! focusedWorkspace
          layout = layoutFun (workspaceLayouts M.! focusedWorkspace) ratio geometry (toList tileable)
          borderedLayout = shrinkWindows borderPx layout

          (floatingPositions, floatMAction, floatRAction) =
            calculateFloatingPositions o floatingWindowsQueue

          newFloatingWindows = BS.insertList focusedWorkspace floatingQueue allWorkspacesFloating
          newFullscreenedWindows = BS.insertList focusedWorkspace fullscreenQueue allWorkspacesFullscreen
          newAllWindows =
            M.unions
              [ (M.fromList $ map (\w -> (winPtr w, w{isFullscreen = True})) fullscreenedWindowsQueue)
              , (M.fromList $ map (\(rect, w) -> (winPtr w, w{isFloating = True, floatingGeometry = Just rect})) (zip floatingPositions floatingWindowsQueue))
              , allWindows
              ]

      -- All manage actions -> executed directly here
      -- 1. Resize tiling windows
      mapM_
        (\(Window{winPtr}, Rect{rw, rh}) -> riverWindowProposeDimensions winPtr rw rh)
        borderedLayout

      -- 2. Resize new floating windows
      floatMAction

      -- 3. Fullscreen windows are already fullscreened

      -- All render actions
      let renderTileNodeActions =
            mapM_
              (\(Window{nodePtr}, Rect{rx, ry}) -> riverNodeSetPosition nodePtr rx ry)
              borderedLayout

          renderBorderActions =
            mapM_ (renderBorder (focusedWindow state) bColor fColor borderPx) nonFullscreenPtrs

          renderActions =
            renderTileNodeActions
              >> lowerAllWindows tileable
              >> raiseAllWindows fullscreenedWindowsQueue
              >> floatRAction
              >> renderBorderActions

      writeIORef
        stateIORef
        state
          { renderQueue = renderQueue state >> renderActions
          , manageQueue = pure ()
          , floatingQueue = []
          , fullscreenQueue = []
          , allWindows = newAllWindows
          , allWorkspacesFloating = newFloatingWindows
          , allWorkspacesFullscreen = newFullscreenedWindows
          }
     where
      getWindows =
        let
          tilingWindowPtrs = (BS.lookupBs focusedWorkspace allWorkspacesTiled)
          floatingWindowPtrs = (BS.lookupBs focusedWorkspace allWorkspacesFloating)
          allW = allWindows
          queueFloatingWindows = fmap (allW M.!) floatingQueue
          queueFullscreenWindows = fmap (allW M.!) fullscreenQueue
          tilingWindows = fmap (allW M.!) tilingWindowPtrs
         in
          (tilingWindows, queueFloatingWindows, queueFullscreenWindows, tilingWindowPtrs S.>< floatingWindowPtrs S.>< (S.fromList floatingQueue))

raiseAllWindows :: (Functor f, Foldable f) => f Window -> IO ()
raiseAllWindows = mapM_ (riverNodePlaceTop . nodePtr)

lowerAllWindows :: (Functor f, Foldable f) => f Window -> IO ()
lowerAllWindows = mapM_ (riverNodePlaceBottom . nodePtr)

shrinkWindows :: CInt -> [(Window, Rect)] -> [(Window, Rect)]
shrinkWindows b =
  map
    ( \(w, r) ->
        ( w
        , r
            { rx = rx r + b
            , ry = ry r + b
            , rh = rh r - 2 * b
            , rw = rw r - 2 * b
            }
        )
    )

renderBorder :: Maybe (Ptr RiverWindow) -> (CUInt, CUInt, CUInt, CUInt) -> (CUInt, CUInt, CUInt, CUInt) -> CInt -> Ptr RiverWindow -> IO ()
renderBorder Nothing (r, g, b, a) _ bPx w = riverWindowSetBorders w edgeAll bPx r g b a
renderBorder (Just focused) (r, g, b, a) (fr, fg, fb, fa) bPx w
  | w == focused = riverWindowSetBorders w edgeAll bPx fr fg fb fa
  | otherwise = riverWindowSetBorders w edgeAll bPx r g b a

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

bColor :: (CUInt, CUInt, CUInt, CUInt)
bColor = translateColor borderColor
fColor :: (CUInt, CUInt, CUInt, CUInt)
fColor = translateColor focusedBorderColor
