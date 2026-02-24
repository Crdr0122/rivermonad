module Handlers.Window where

import Control.Monad (when)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe
import Foreign
import Foreign.C
import Types
import Utils.BiMap qualified as B
import Wayland.ImportedFunctions

foreign export ccall "hs_window_closed"
  hsWindowClosed :: Ptr () -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_window_dimensions"
  hsWindowDimensions :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> IO ()

hsWindowClosed :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowClosed dataPtr win = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  let newWindows = M.delete win (allWindows state)
      newWorkspaces = B.delete win (allWorkspacesTiled state)
      newWorkspacesFloating = B.delete win (allWorkspacesFloating state)
      f = focusedWindow state
      newFocusedWin
        | isNothing f = Nothing
        | fromJust f == win = Nothing
        | otherwise = f
  writeIORef
    stateIORef
    state
      { allWindows = newWindows
      , allWorkspacesTiled = newWorkspaces
      , allWorkspacesFloating = newWorkspacesFloating
      , focusedWindow = newFocusedWin
      }
  riverWindowDestroy win

hsWindowDimensions :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> IO ()
hsWindowDimensions dataPtr winP width height = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  let w = allWindows state M.! winP
  when (isFloating w) $ do
    let newWindow = w{floatingHeight = height, floatingWidth = width}
        newAllWindows = M.insert winP newWindow (allWindows state)
    writeIORef stateIORef state{allWindows = newAllWindows}
