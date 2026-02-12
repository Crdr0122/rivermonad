module Layout where

import Data.Foldable
import Data.Map.Strict qualified as M
import Data.Maybe
import Types
import Utils.BiMap qualified as B
import Wayland.ImportedFunctions

startLayout :: WMState -> IO (IO ())
startLayout state = do
  let mOut = M.lookup (focusedOutput state) (allOutputs state)
  case mOut of
    Nothing -> return (pure ())
    Just out -> do
      let tileable = getTileableWindows state
          geometry = Rect{rx = outX out, ry = outY out, rh = outHeight out, rw = outWidth out}
          layout = calculateStack geometry tileable

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

      return renderActions

getTileableWindows :: WMState -> [Window]
getTileableWindows state =
  let
    windowPtrs = toList (B.lookupBs (focusedWorkspace state) (allWorkspaces state))
    windows = map ((allWindows state) M.!) windowPtrs
   in
    filter (\w -> not (isFloating w) && not (isFullscreen w)) windows

calculateStack :: Rect -> [Window] -> [(Window, Rect)]
calculateStack _ [] = []
calculateStack total [w] = [(w, total)] -- Only one window? It gets the whole screen
calculateStack total (master : slaves) =
  let masterWidth = rw total `div` 2
      masterRect = total{rw = masterWidth}
      stackRect = total{rx = rx total + masterWidth, rw = rw total - masterWidth}
      slaveHeight = rh stackRect `div` length slaves

      -- Map over slaves to give them each a slice of the stack height
      slaveGeos =
        zipWith
          (\w i -> (w, stackRect{ry = ry stackRect + (i * slaveHeight), rh = slaveHeight}))
          slaves
          [0 ..]
   in (master, masterRect) : slaveGeos
