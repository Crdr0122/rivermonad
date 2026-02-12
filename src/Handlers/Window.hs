module Handlers.Window where

import Control.Monad (when)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe
import Foreign
import Layout
import Types
import Utils.BiMap qualified as B
import Wayland.Client
import Wayland.ImportedFunctions

foreign export ccall "hs_window_closed"
  hsWindowClosed :: Ptr () -> Ptr RiverWindow -> IO ()

hsWindowClosed :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowClosed dataPtr win = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  let newWindows = M.delete win (allWindows state)
      newWorkspaces = B.delete win (allWorkspaces state)
      f = focusedWindow state
      newFocusedWin
        | isNothing f = Nothing
        | fromJust f == win = Nothing
        | otherwise = f
  writeIORef
    stateIORef
    state
      { allWindows = newWindows
      , allWorkspaces = newWorkspaces
      , focusedWindow = newFocusedWin
      }
  riverWindowDestroy win
