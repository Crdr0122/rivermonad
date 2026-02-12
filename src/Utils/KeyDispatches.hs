module Utils.KeyDispatches where

import Data.IORef
import Types
import Wayland.ImportedFunctions

closeCurrentWindow :: IORef WMState -> IO ()
closeCurrentWindow stateIORef = do
  state <- readIORef stateIORef
  let closeWindow (Just w) = riverWindowClose w
      closeWindow Nothing = pure ()
  writeIORef stateIORef state{manageQueue = manageQueue state >> closeWindow (focusedWindow state)}
