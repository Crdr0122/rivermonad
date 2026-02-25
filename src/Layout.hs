module Layout where

import Config
import Data.Foldable
import Data.Map.Strict qualified as M
import Foreign
import Foreign.C
import Types
import Utils.BiMap qualified as B
import Wayland.ImportedFunctions

startLayout :: WMState -> IO (IO ())
startLayout state = do
  let mOut = M.lookup (focusedOutput state) (allOutputs state)
  case mOut of
    Nothing -> pure (pure ())
    Just out -> do
      let (tileable, fullscreened, allNeededWindowPtrs) = getTileableWindows state
          geometry = Rect{rx = outX out, ry = outY out, rh = outHeight out, rw = outWidth out}
          layout = layoutFun (workspaceLayouts state M.! focusedWorkspace state) geometry tileable
          borderedLayout = shrinkWindows (fromIntegral borderPx) layout

      -- Step A: Send manage requests immediately
      -- River expects propose_dimensions during the manage cycle
      mapM_
        ( \(w, rect) ->
            riverWindowProposeDimensions (winPtr w) (fromIntegral $ rw rect) (fromIntegral $ rh rect)
        )
        borderedLayout

      -- Step B: Return the render actions for the renderQueue
      -- These will be executed when the compositor sends a render event
      let renderNodeActions =
            mapM_
              ( \(w, rect) ->
                  riverNodeSetPosition (nodePtr w) (fromIntegral $ rx rect) (fromIntegral $ ry rect)
              )
              borderedLayout

          renderBorderActions =
            mapM_ (renderBorder (focusedWindow state) (unpackRGBA borderColor) (unpackRGBA focusedBorderColor)) allNeededWindowPtrs

      return $
        renderNodeActions >> lowerAllWindows tileable >> raiseAllWindows fullscreened >> renderBorderActions

getTileableWindows :: WMState -> ([Window], [Window], [Ptr RiverWindow])
getTileableWindows state =
  let
    windowPtrs = toList (B.lookupBs (focusedWorkspace state) (allWorkspacesTiled state))
    windowFloatingPtrs = toList (B.lookupBs (focusedWorkspace state) (allWorkspacesFloating state))
    windows = map ((allWindows state) M.!) windowPtrs
    floatingWindows = map ((allWindows state) M.!) windowFloatingPtrs
    tileable = filter (not . isFullscreen) windows
    fullscreened = filter isFullscreen windows ++ filter isFullscreen floatingWindows
   in
    (tileable, fullscreened, windowPtrs ++ windowFloatingPtrs)

raiseAllWindows :: [Window] -> IO ()
raiseAllWindows = mapM_ (\w -> riverNodePlaceTop (nodePtr w))

lowerAllWindows :: [Window] -> IO ()
lowerAllWindows = mapM_ (\w -> riverNodePlaceBottom (nodePtr w))

shrinkWindows :: Int -> [(Window, Rect)] -> [(Window, Rect)]
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

renderBorder :: Maybe (Ptr RiverWindow) -> (CUInt, CUInt, CUInt, CUInt) -> (CUInt, CUInt, CUInt, CUInt) -> Ptr RiverWindow -> IO ()
renderBorder Nothing (r, g, b, a) _ w = riverWindowSetBorders w edgeAll borderPx r g b a
renderBorder (Just focused) (r, g, b, a) (fg, fr, fb, fa) w
  | w == focused = riverWindowSetBorders w edgeAll borderPx fr fg fb fa
  | otherwise = riverWindowSetBorders w edgeAll borderPx r g b a

unpackRGBA :: Word32 -> (CUInt, CUInt, CUInt, CUInt)
unpackRGBA rgba =
  ( fromIntegral ((rgba `shiftR` 24) .&. 0xFF) -- R
  , fromIntegral ((rgba `shiftR` 16) .&. 0xFF) -- G
  , fromIntegral ((rgba `shiftR` 8) .&. 0xFF) -- B
  , fromIntegral (rgba .&. 0xFF) -- A
  )
