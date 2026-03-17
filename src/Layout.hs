module Layout (startLayout) where

import Config
import Control.Concurrent.MVar
import Control.Monad (unless)
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
  modifyMVar_ stateMVar $ \state -> do
    let
      newWindows = (allWindows state M.!) <$> newWindowQueue state
      focusedWorkspace = fromMaybe 1 $ B.lookup (focusedOutput state) (allOutputWorkspaces state)
      checkWorkspaceRule window@Window{winTitle, winAppID} =
        case find (\(t, a, _) -> t `isInfixOf` winTitle && a `isInfixOf` winAppID) (workspaceRules myConfig) of
          Nothing -> (window, focusedWorkspace)
          Just (_, _, workspace) -> (window, workspace)

      checkTilingRule (window@Window{winTitle, winAppID}, workspace) =
        case find (\(t, a, _) -> t `isInfixOf` winTitle && a `isInfixOf` winAppID) (floatingRules myConfig) of
          Nothing -> (window, workspace, Tiled)
          Just (_, _, s) -> (window, workspace, s)

      divided = checkTilingRule <$> (checkWorkspaceRule <$> newWindows)

      (newTiled, newFloating, newFullscreen) =
        foldl'
          ( \(oldTiled, oldFloating, oldFullscreen) (Window{winPtr}, w, s) ->
              case s of
                Tiled -> (BS.insert w winPtr oldTiled, oldFloating, oldFullscreen)
                Floating -> (oldTiled, M.adjust (winPtr :) w oldFloating, oldFullscreen)
                Fullscreen -> (oldTiled, oldFloating, M.adjust (winPtr :) w oldFullscreen)
                FullscreenFloating -> (oldTiled, oldFloating, M.adjust (winPtr :) w oldFullscreen)
          )
          (allWorkspacesTiled state, floatingQueue state, fullscreenQueue state)
          divided

      (newFocused, seatFocus) = case find (\(_, i, _) -> i == focusedWorkspace) divided of
        Nothing -> (focusedWindow state, pure ())
        Just (Window{winPtr}, _, _) -> (Just winPtr, riverSeatFocusWindow (focusedSeat state) winPtr)

      hideWindows = mapM_ (\(Window{winPtr}, workspace, _) -> unless (workspace `elem` B.keysR (allOutputWorkspaces state)) (riverWindowHide winPtr)) divided

    seatFocus

    pure
      state
        { floatingQueue = newFloating
        , allWorkspacesTiled = newTiled
        , fullscreenQueue = newFullscreen
        , newWindowQueue = []
        , focusedWindow = newFocused
        , renderQueue = renderQueue state >> hideWindows
        }

  state <- readMVar stateMVar
  let outputs = B.toList (allOutputWorkspaces state)
  mapM_ (\(o, w) -> startLayoutOutput o w stateMVar) outputs

startLayoutOutput :: Ptr RiverOutput -> WorkspaceID -> MVar WMState -> IO ()
startLayoutOutput output focusedWorkspace stateMVar = do
  modifyMVar_ stateMVar $
    \state@WMState
       { allOutputs
       , floatingQueue
       , fullscreenQueue
       , workspaceLayouts
       , allWorkspacesFloating
       , allWorkspacesTiled
       , allWorkspacesFullscreen
       , allWindows
       , focusedWindow
       } -> do
        let mOut = M.lookup output allOutputs
        case mOut of
          Nothing -> pure state
          Just o@Output{outX, outY, outHeight, outWidth} -> do
            let
              tilingWindowPtrs = (BS.lookupBs focusedWorkspace allWorkspacesTiled)
              floatingWindowPtrs = (BS.lookupBs focusedWorkspace allWorkspacesFloating)
              tileable = (allWindows M.!) <$> tilingWindowPtrs
              floatingWindow = (allWindows M.!) <$> floatingWindowPtrs
              floatingQueuedWindows = (allWindows M.!) <$> S.fromList (floatingQueue M.! focusedWorkspace)
              nonFullscreen = tileable S.>< floatingWindow S.>< floatingQueuedWindows
              geometry = Rect{rx = outX, ry = outY, rh = outHeight, rw = outWidth}

              indexFocusedWindow = case focusedWindow of
                Nothing -> Nothing
                Just focused -> S.elemIndexL focused tilingWindowPtrs

              layout = applySomeLayout (workspaceLayouts M.! focusedWorkspace) indexFocusedWindow geometry tileable
              gappedLayout = shrinkWindows (gapPx myConfig) (toList layout)
              borderedLayout = shrinkWindows (borderPx myConfig) gappedLayout

              newFloatingWindows = (allWindows M.!) <$> (floatingQueue M.! focusedWorkspace)
              (floatingPositions, floatMAction, floatRAction) = calculateFloatingPositions o newFloatingWindows

              newFullscreenWindows =
                (allWindows M.!) <$> (fullscreenQueue M.! focusedWorkspace)
              newWorkspacesFloating =
                BS.insertList focusedWorkspace (winPtr <$> newFloatingWindows) allWorkspacesFloating
              newWorkspacesFullscreen =
                BS.insertList focusedWorkspace (winPtr <$> newFullscreenWindows) allWorkspacesFullscreen

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
                        toList $
                          fmap
                            (\(w, rect) -> (winPtr w, w{tilingGeometry = Just rect}))
                            borderedLayout
                    )
                  , allWindows
                  ]

            -- All manage actions -> executed directly here
            -- 1. Resize tiling windows
            mapM_ (\(Window{winPtr}, Rect{rw, rh}) -> riverWindowProposeDimensions winPtr rw rh) borderedLayout

            -- 2. Resize new floating windows
            floatMAction

            -- 3. Only fullscreen newly fullscreened windows
            mapM_ (\Window{winPtr} -> riverWindowFullscreen winPtr output >> riverWindowInformFullscreen winPtr) newFullscreenWindows

            -- All render actions
            let renderTileActions =
                  mapM_
                    ( \(Window{nodePtr, winPtr}, Rect{rx, ry, rw, rh}) ->
                        riverNodeSetPosition nodePtr rx ry >> riverWindowSetContentClipBox winPtr 0 0 rw rh >> riverNodePlaceBottom nodePtr
                    )
                    borderedLayout

                renderBorderActions =
                  mapM_ (renderBorder focusedWindow bColor fColor pColor (borderPx myConfig)) nonFullscreen

                freeFloatingClipbox =
                  mapM_
                    (\Window{winPtr} -> riverWindowSetContentClipBox winPtr 0 0 0 0)
                    newFloatingWindows

                renderActions =
                  renderTileActions
                    >> floatRAction
                    >> freeFloatingClipbox
                    >> renderBorderActions
                    >> raiseAllWindows (reverse newFullscreenWindows) -- Fullscreen windows are at the top -> First one raise to top last
            pure
              state
                { renderQueue = renderQueue state >> renderActions
                , manageQueue = pure ()
                , floatingQueue = M.insert focusedWorkspace [] floatingQueue
                , fullscreenQueue = M.insert focusedWorkspace [] fullscreenQueue
                , newWindowQueue = []
                , allWindows = newAllWindows
                , allWorkspacesFloating = newWorkspacesFloating
                , allWorkspacesFullscreen = newWorkspacesFullscreen
                }

raiseAllWindows :: (Functor f, Foldable f) => f Window -> IO ()
raiseAllWindows = mapM_ (riverNodePlaceTop . nodePtr)

-- lowerAllWindows :: (Functor f, Foldable f) => f Window -> IO ()
-- lowerAllWindows = mapM_ (riverNodePlaceBottom . nodePtr)

shrinkWindows :: CInt -> [(Window, Rect)] -> [(Window, Rect)]
shrinkWindows b =
  fmap
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
