module Handlers.Window where

import Control.Monad (when)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as S
import Foreign
import Foreign.C
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.ImportedFunctions

foreign export ccall "hs_window_closed"
  hsWindowClosed :: Ptr () -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_window_dimensions"
  hsWindowDimensions :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> IO ()
foreign export ccall "hs_window_parent"
  hsWindowParent :: Ptr () -> Ptr RiverWindow -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_window_dimensions_hint"
  hsWindowDimensionsHint :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()

hsWindowClosed :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowClosed dataPtr win = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  let newWindows = M.delete win (allWindows state)
      newWorkspaces = BS.delete win (allWorkspacesTiled state)
      newWorkspacesFloating = BS.delete win (allWorkspacesFloating state)
      remTiled = BS.lookupBs (focusedWorkspace state) newWorkspaces
      remFloating = BS.lookupBs (focusedWorkspace state) newWorkspacesFloating
      f = focusedWindow state
      newFocusedWin
        | isNothing f = Nothing
        | fromJust f /= win = f
        | otherwise = case remTiled of
            w S.:<| _ -> Just w
            S.Empty -> case remFloating of
              w S.:<| _ -> Just w
              S.Empty -> Nothing
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
  state@WMState{opDeltaState} <- readIORef stateIORef
  case opDeltaState of
    None -> do
      let w@Window{isFloating, floatingGeometry, isFullscreen} = allWindows state M.! winP
      when (isFloating && not isFullscreen) $ do
        let newGeometry =
              (fromMaybe (Rect 0 0 0 0) floatingGeometry)
                { rw = width
                , rh = height
                }
            newWindow = w{floatingGeometry = Just newGeometry}
            newAllWindows = M.insert winP newWindow (allWindows state)
        writeIORef stateIORef state{allWindows = newAllWindows}
    _ -> pure ()

hsWindowParent :: Ptr () -> Ptr RiverWindow -> Ptr RiverWindow -> IO ()
hsWindowParent dataPtr win parent = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  let
    o = allOutputs state M.! focusedOutput state
    window = (allWindows state M.! win)
    (calcPos, mAction, rAction) = calculateFloatingPosition win window{parentWindow = Just parent} o
    newWindows =
      M.adjust
        (\w -> w{isFloating = True, floatingGeometry = Just calcPos, parentWindow = Just parent})
        win
        (allWindows state)
    newTiled = BS.delete win (allWorkspacesTiled state)
    newFloating = BS.insert (focusedWorkspace state) win (allWorkspacesFloating state)

  writeIORef
    stateIORef
    state
      { allWindows = newWindows
      , allWorkspacesFloating = newFloating
      , allWorkspacesTiled = newTiled
      , manageQueue = manageQueue state >> mAction
      , renderQueue = renderQueue state >> rAction
      }
  riverWindowManagerManageDirty (currentWmManager state)

hsWindowDimensionsHint :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
hsWindowDimensionsHint dataPtr win minW minH maxW maxH = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state@WMState{allWindows} <- readIORef stateIORef
  let newWindows = M.adjust (\w -> w{dimensionsHint = (minW, minH, maxW, maxH)}) win allWindows
  writeIORef stateIORef state{allWindows = newWindows}
