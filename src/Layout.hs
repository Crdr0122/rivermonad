module Layout (startLayout) where

import Config
import Control.Concurrent.MVar
import Data.Bimap qualified as B
import Data.Foldable
import Data.List
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as S
import Foreign
import Foreign.C
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.ImportedFunctions

startLayout :: MVar WMState -> IO ()
startLayout stateMVar = do
  state <- readMVar stateMVar
  let outputs = B.toList (allOutputWorkspaces state)
  mapM_ (\(o, w) -> startLayoutOutput o w stateMVar) outputs

startLayoutOutput :: Ptr RiverOutput -> WorkspaceID -> MVar WMState -> IO ()
startLayoutOutput output focusedWorkspace stateMVar = do
  modifyMVar_ stateMVar $
    \state@WMState
       { allOutputs
       , workspaceRatios
       , floatingQueue
       , newWindowQueue
       , workspaceLayouts
       , allWorkspacesFloating
       , allWorkspacesTiled
       , allWorkspacesFullscreen
       , allWindows
       } -> do
        let mOut = M.lookup output allOutputs
        case mOut of
          Nothing -> pure state
          Just o@Output{outX, outY, outHeight, outWidth} -> do
            let (tileable, floatingWindowsQueue, nonFullscreenPtrs) = getWindows
                geometry = Rect{rx = outX, ry = outY, rh = outHeight, rw = outWidth}
                ratio = workspaceRatios M.! focusedWorkspace

                (newQueuedFloatingWindows, newTiledWindows, newFullscreenWindows) =
                  ruleNewWindows (fmap (allWindows M.!) newWindowQueue)

                windowsToTile = newTiledWindows ++ toList tileable

                layout = layoutFun (workspaceLayouts M.! focusedWorkspace) ratio geometry windowsToTile
                gappedLayout = shrinkWindows gapPx layout
                borderedLayout = shrinkWindows borderPx gappedLayout

                newFloatingWindows = newQueuedFloatingWindows ++ floatingWindowsQueue

                (floatingPositions, floatMAction, floatRAction) = calculateFloatingPositions o newFloatingWindows

                newWorkspacesFloating =
                  BS.insertList focusedWorkspace (fmap winPtr newFloatingWindows ++ floatingQueue) allWorkspacesFloating
                newWorkspacesTiled =
                  BS.insertList focusedWorkspace (fmap winPtr newTiledWindows) allWorkspacesTiled
                newWorkspacesFullscreen =
                  BS.insertList focusedWorkspace (fmap winPtr newFullscreenWindows) allWorkspacesFullscreen

                newAllWindows =
                  M.unions
                    [ ( M.fromList $
                          map
                            (\(rect, w) -> (winPtr w, w{isFloating = True, floatingGeometry = Just rect}))
                            (zip floatingPositions newFloatingWindows)
                      )
                    , ( M.fromList $
                          map
                            (\w -> (winPtr w, w{isFullscreen = True}))
                            newFullscreenWindows
                      )
                    , ( M.fromList $
                          map
                            (\(w, rect) -> (winPtr w, w{tilingGeometry = Just rect}))
                            borderedLayout
                      )
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
            mapM_ (\w -> riverWindowFullscreen (winPtr w) output) newFullscreenWindows

            -- All render actions
            let renderTileNodeActions =
                  mapM_
                    (\(Window{nodePtr}, Rect{rx, ry}) -> riverNodeSetPosition nodePtr rx ry)
                    borderedLayout

                renderBorderActions =
                  mapM_ (renderBorder (focusedWindow state) bColor fColor borderPx) nonFullscreenPtrs

                renderActions =
                  renderTileNodeActions
                    >> lowerAllWindows windowsToTile
                    >> floatRAction
                    >> renderBorderActions
                    >> raiseAllWindows newFullscreenWindows

            pure
              state
                { renderQueue = renderQueue state >> renderActions
                , manageQueue = pure ()
                , floatingQueue = []
                , newWindowQueue = []
                , allWindows = newAllWindows
                , allWorkspacesFloating = newWorkspacesFloating
                , allWorkspacesTiled = newWorkspacesTiled
                , allWorkspacesFullscreen = newWorkspacesFullscreen
                }
           where
            ruleNewWindows xs =
              let (float, tiled) =
                    partition
                      ( \Window{winTitle, winAppID, dimensionsHint = (_, _, maxW, maxH), parentWindow} ->
                          any (\(t, a) -> t `isInfixOf` winTitle && a `isInfixOf` winAppID) floatingRules
                            || isJust parentWindow
                      )
                      xs
               in (float, tiled, [])
            getWindows =
              let
                tilingWindowPtrs = (BS.lookupBs focusedWorkspace allWorkspacesTiled)
                floatingWindowPtrs = (BS.lookupBs focusedWorkspace allWorkspacesFloating)
                allW = allWindows
                queueFloatingWindows = fmap (allW M.!) floatingQueue
                tilingWindows = fmap (allW M.!) tilingWindowPtrs
               in
                (tilingWindows, queueFloatingWindows, tilingWindowPtrs S.>< floatingWindowPtrs S.>< (S.fromList floatingQueue))

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
