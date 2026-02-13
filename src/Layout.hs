module Layout where

import Data.Foldable
import Data.Map.Strict qualified as M
import Types
import Utils.BiMap qualified as B
import Wayland.ImportedFunctions

startLayout :: WMState -> IO (IO ())
startLayout state = do
  let mOut = M.lookup (focusedOutput state) (allOutputs state)
  case mOut of
    Nothing -> pure (pure ())
    Just out -> do
      let (tileable, fullscreened) = getTileableWindows state
          geometry = Rect{rx = outX out, ry = outY out, rh = outHeight out, rw = outWidth out}
          layout = layoutFun (workspaceLayouts state M.! focusedWorkspace state) geometry tileable

      -- Step A: Send manage requests immediately
      -- River expects propose_dimensions during the manage cycle
      mapM_
        ( \(w, rect) ->
            riverWindowProposeDimensions (winPtr w) (fromIntegral $ rw rect) (fromIntegral $ rh rect)
        )
        layout

      -- Step B: Return the render actions for the renderQueue
      -- These will be executed when the compositor sends a render event
      let renderActions =
            mapM_
              ( \(w, rect) ->
                  riverNodeSetPosition (nodePtr w) (fromIntegral $ rx rect) (fromIntegral $ ry rect)
              )
              layout

      return $
        renderActions >> lowerAllWindows tileable >> raiseAllWindows fullscreened

getTileableWindows :: WMState -> ([Window], [Window])
getTileableWindows state =
  let
    windowPtrs = toList (B.lookupBs (focusedWorkspace state) (allWorkspacesTiled state))
    windowFloatingPtrs = toList (B.lookupBs (focusedWorkspace state) (allWorkspacesFloating state))
    windows = map ((allWindows state) M.!) windowPtrs
    floatingWindows = map ((allWindows state) M.!) windowFloatingPtrs
    tileable = filter (not . isFullscreen) windows
    fullscreened = filter isFullscreen windows ++ filter isFullscreen floatingWindows
   in
    (tileable, fullscreened)

raiseAllWindows :: [Window] -> IO ()
raiseAllWindows = mapM_ (\w -> riverNodePlaceTop (nodePtr w))

lowerAllWindows :: [Window] -> IO ()
lowerAllWindows = mapM_ (\w -> riverNodePlaceBottom (nodePtr w))
