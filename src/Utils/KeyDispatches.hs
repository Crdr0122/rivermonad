module Utils.KeyDispatches where

import Control.Monad (unless)
import Data.IORef
import Data.List
import Data.Map qualified as M
import Data.Sequence qualified as S
import System.Process
import Types
import Utils.BiMap qualified as B
import Wayland.ImportedFunctions

closeCurrentWindow :: IORef WMState -> IO ()
closeCurrentWindow stateIORef = do
  state <- readIORef stateIORef
  let closeWindow (Just w) = riverWindowClose w
      closeWindow Nothing = pure ()
  writeIORef stateIORef state{manageQueue = manageQueue state >> closeWindow (focusedWindow state)}

toggleFullscreenCurrentWindow :: IORef WMState -> IO ()
toggleFullscreenCurrentWindow stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just win -> do
      let toggle x = x{isFullscreen = not (isFullscreen x)}
          newWindows = M.adjust toggle win (allWindows state)
          w = allWindows state M.! win
          action =
            if isFullscreen w
              then
                riverWindowExitFullscreen win
              else
                riverWindowFullscreen win (focusedOutput state)
      riverWindowManagerManageDirty (currentWmManager state)
      writeIORef stateIORef state{allWindows = newWindows, manageQueue = manageQueue state >> action}

cycleWindows :: IORef WMState -> IO ()
cycleWindows stateIORef = do
  state <- readIORef stateIORef
  let
    work = focusedWorkspace state
    oldTiledWindows = B.lookupBs work $ allWorkspacesTiled state
  unless (S.null oldTiledWindows) $
    case S.viewr oldTiledWindows of
      S.EmptyR -> pure ()
      hs S.:> h ->
        writeIORef
          stateIORef
          state
            { allWorkspacesTiled = B.insertSeq work (h S.<| hs) (allWorkspacesTiled state)
            }

cycleWindowsBack :: IORef WMState -> IO ()
cycleWindowsBack stateIORef = do
  state <- readIORef stateIORef
  let
    work = focusedWorkspace state
    oldTiledWindows = B.lookupBs work $ allWorkspacesTiled state
  unless (S.null oldTiledWindows) $
    case S.viewr oldTiledWindows of
      S.EmptyR -> pure ()
      hs S.:> h ->
        writeIORef
          stateIORef
          state
            { allWorkspacesTiled = B.insertSeq work (h S.<| hs) (allWorkspacesTiled state)
            }

switchWorkspace :: WorkspaceID -> IORef WMState -> IO ()
switchWorkspace workspaceID stateIORef = do
  state <- readIORef stateIORef
  let
    currentFocused = focusedWorkspace state
  unless (workspaceID == currentFocused) $ do
    let
      currentWindowsTiled = B.lookupBs currentFocused (allWorkspacesTiled state)
      currentWindowsFloating = B.lookupBs currentFocused (allWorkspacesFloating state)
      newWindowsTiled = B.lookupBs workspaceID (allWorkspacesTiled state)
      newWindowsFloating = B.lookupBs workspaceID (allWorkspacesFloating state)

      hidingActions = mapM_ riverWindowHide currentWindowsTiled >> mapM_ riverWindowHide currentWindowsFloating
      showingActions = mapM_ riverWindowShow newWindowsTiled >> mapM_ riverWindowShow newWindowsFloating

    writeIORef
      stateIORef
      state
        { renderQueue = renderQueue state >> hidingActions >> showingActions
        , focusedWorkspace = workspaceID
        }
    riverWindowManagerManageDirty (currentWmManager state)

cycleLayout :: [LayoutType] -> IORef WMState -> IO ()
cycleLayout [] _ = pure ()
cycleLayout layouts@(x : _) stateIORef = do
  state <- readIORef stateIORef
  let oldWorkspaceLayouts = workspaceLayouts state
      curr = focusedWorkspace state
      currentLayout = layoutName $ oldWorkspaceLayouts M.! curr
  case elemIndex currentLayout (map layoutName layouts) of
    Nothing -> pure ()
    Just i -> do
      let
        nextLayout = if i + 1 < length layouts then layouts !! (i + 1) else x
        newWorkspaceLayouts = M.insert curr nextLayout oldWorkspaceLayouts
      writeIORef stateIORef state{workspaceLayouts = newWorkspaceLayouts}
      riverWindowManagerManageDirty $ currentWmManager state

exec :: String -> IORef WMState -> IO ()
exec command _ = spawnCommand command >> pure ()
