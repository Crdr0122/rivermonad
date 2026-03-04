module Handlers.Window where

import Control.Monad (unless, when)
import Data.Bimap qualified as B
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Maybe
import Foreign
import Foreign.C
import Types
import Utils.BiSeqMap qualified as BS
import Wayland.ImportedFunctions

foreign export ccall "hs_window_closed"
  hsWindowClosed :: Ptr () -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_window_dimensions"
  hsWindowDimensions :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> IO ()
foreign export ccall "hs_window_parent"
  hsWindowParent :: Ptr () -> Ptr RiverWindow -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_window_dimensions_hint"
  hsWindowDimensionsHint :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign export ccall "hs_window_title"
  hsWindowTitle :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
foreign export ccall "hs_window_app_id"
  hsWindowAppID :: Ptr () -> Ptr RiverWindow -> CString -> IO ()

hsWindowClosed :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowClosed dataPtr win = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  let newWindows = M.delete win (allWindows state)
      newWorkspacesTiled = BS.delete win (allWorkspacesTiled state)
      newWorkspacesFloating = BS.delete win (allWorkspacesFloating state)
      newWorkspacesFullscreen = BS.delete win (allWorkspacesFullscreen state)
      f = focusedWindow state
      newFocusedWin
        | isNothing f = Nothing
        | fromJust f /= win = f
        | otherwise = Nothing
  writeIORef
    stateIORef
    state
      { allWindows = newWindows
      , allWorkspacesTiled = newWorkspacesTiled
      , allWorkspacesFullscreen = newWorkspacesFullscreen
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
      let w@Window{isFloating, floatingGeometry, isFullscreen, tilingGeometry} = allWindows state M.! winP
      unless isFullscreen $
        if isFloating
          then do
            let newGeometry =
                  (fromMaybe (Rect 0 0 0 0) floatingGeometry)
                    { rw = width
                    , rh = height
                    }
                newWindow = w{floatingGeometry = Just newGeometry}
                newAllWindows = M.insert winP newWindow (allWindows state)
            writeIORef stateIORef state{allWindows = newAllWindows}
          else do
            let newGeometry =
                  (fromMaybe (Rect 0 0 0 0) tilingGeometry)
                    { rw = width
                    , rh = height
                    }
                newWindow = w{tilingGeometry = Just newGeometry}
                newAllWindows = M.insert winP newWindow (allWindows state)
            writeIORef stateIORef state{allWindows = newAllWindows}
    _ -> pure ()

hsWindowParent :: Ptr () -> Ptr RiverWindow -> Ptr RiverWindow -> IO ()
hsWindowParent dataPtr win parent = do
  print "Has Parent"
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  let newWindows = M.adjust (\w -> w{parentWindow = Just parent}) win (allWindows state)
  writeIORef
    stateIORef
    state{allWindows = newWindows}

hsWindowDimensionsHint :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
hsWindowDimensionsHint dataPtr win minW minH maxW maxH = do
  print $ "Dimensions hint" ++ show (minW, minH, maxW, maxH)
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state@WMState{allWindows} <- readIORef stateIORef
  let newWindows = M.adjust (\w -> w{dimensionsHint = (minW, minH, maxW, maxH)}) win allWindows
      newState = state{allWindows = newWindows}
  if minW == maxW && minH == maxH && minW /= 0 && minH /= 0
    then do
      let newTiled = BS.delete win (allWorkspacesTiled state)
          newFullscreen = BS.delete win (allWorkspacesFullscreen state)
      writeIORef
        stateIORef
        newState
          { allWorkspacesTiled = newTiled
          , allWorkspacesFullscreen = newFullscreen
          , floatingQueue = win : floatingQueue state
          }
    else writeIORef stateIORef newState

hsWindowTitle :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
hsWindowTitle dataPtr win title = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  t <- peekCString title
  let newWindows = M.adjust (\w -> w{winTitle = t}) win (allWindows state)
  writeIORef stateIORef state{allWindows = newWindows}

hsWindowAppID :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
hsWindowAppID dataPtr win appID = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  a <- peekCString appID
  let newWindows = M.adjust (\w -> w{winAppID = a}) win (allWindows state)
  writeIORef stateIORef state{allWindows = newWindows}

hsWindowFullscreenRequested :: Ptr () -> Ptr RiverWindow -> Ptr RiverOutput -> IO ()
hsWindowFullscreenRequested dataPtr win output = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state@WMState
    { allWindows
    , allWorkspacesFloating
    , allWorkspacesTiled
    , allWorkspacesFullscreen
    } <-
    readIORef stateIORef
  let window@Window{isFloating, nodePtr} = allWindows M.! win
      newWindows = M.insert win window{isFullscreen = True} allWindows
      newState
        | isFloating = state{allWorkspacesFloating = BS.delete win allWorkspacesFloating}
        | otherwise = state{allWorkspacesTiled = BS.delete win allWorkspacesTiled}
      targetWorkspace = allOutputWorkspaces state B.! output

  writeIORef
    stateIORef
    newState
      { allWindows = newWindows
      , manageQueue = manageQueue state >> riverWindowFullscreen win output
      , renderQueue = renderQueue state >> riverNodePlaceTop nodePtr
      , allWorkspacesFullscreen = BS.insert targetWorkspace win allWorkspacesFullscreen
      }

hsWindowExitFullscreenRequested :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowExitFullscreenRequested dataPtr win = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state@WMState
    { allWindows
    , allWorkspacesFloating
    , allWorkspacesTiled
    , allWorkspacesFullscreen
    , focusedOutput
    , allOutputWorkspaces
    } <-
    readIORef stateIORef
  let window@Window{isFloating} = allWindows M.! win
      newWindows = M.insert win window{isFullscreen = False} allWindows
      workspace = fromMaybe (allOutputWorkspaces B.! focusedOutput) (BS.lookupA win allWorkspacesFullscreen)
      newState
        | isFloating = state{allWorkspacesFloating = BS.insert workspace win allWorkspacesFloating}
        | otherwise = state{allWorkspacesTiled = BS.insert workspace win allWorkspacesTiled}
  writeIORef
    stateIORef
    newState
      { allWindows = newWindows
      , manageQueue = manageQueue state >> riverWindowExitFullscreen win
      , allWorkspacesFullscreen = BS.delete win allWorkspacesFullscreen
      }
