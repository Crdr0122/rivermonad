module Utils.KeyDispatches where

import Control.Monad (unless)
import Data.IORef
import Data.List
import Data.Map qualified as M
import Data.Sequence qualified as S
import System.Process
import Types
import Utils.BiSeqMap qualified as BS
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

toggleFloatingCurrentWindow :: IORef WMState -> IO ()
toggleFloatingCurrentWindow stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just win -> if isFloating (allWindows state M.! win) then tileCurrentWindow stateIORef else floatCurrentWindow stateIORef

floatCurrentWindow :: IORef WMState -> IO ()
floatCurrentWindow stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just win -> do
      let
        o = allOutputs state M.! focusedOutput state
        oldWindow = (allWindows state M.! win)
        (calcPos, mAction, rAction) =
          ( Rect
              { rx = offsetX
              , ry = offsetY
              , rw = w
              , rh = h
              }
          , riverWindowProposeDimensions win (fromIntegral w) (fromIntegral h)
          , riverNodeSetPosition
              (nodePtr oldWindow)
              (fromIntegral (offsetX + outX o))
              (fromIntegral (offsetY + outY o))
          )
         where
          w = outWidth o * 6 `div` 10
          h = outHeight o * 6 `div` 10
          offsetX = (outWidth o - w) `div` 2
          offsetY = (outHeight o - h) `div` 2
        (newGeo) = case floatingGeometry oldWindow of
          Nothing -> calcPos
          Just g -> g
        newWindows =
          M.adjust
            (\w -> w{isFloating = True, floatingGeometry = Just newGeo})
            win
            (allWindows state)
        newTiled = B.delete win (allWorkspacesTiled state)
        newFloating = B.insert (focusedWorkspace state) win (allWorkspacesFloating state)

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

tileCurrentWindow :: IORef WMState -> IO ()
tileCurrentWindow stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just win -> do
      let tile x = x{isFloating = False}
          newWindows = M.adjust tile win (allWindows state)
          newFloating = BS.delete win (allWorkspacesFloating state)
          newTiled = BS.insert (focusedWorkspace state) win (allWorkspacesTiled state)

      riverWindowManagerManageDirty (currentWmManager state)
      writeIORef stateIORef state{allWindows = newWindows, allWorkspacesFloating = newFloating, allWorkspacesTiled = newTiled}

cycleWindows :: IORef WMState -> IO ()
cycleWindows stateIORef = do
  state <- readIORef stateIORef
  let
    work = focusedWorkspace state
    oldTiledWindows = BS.lookupBs work $ allWorkspacesTiled state
  unless (S.null oldTiledWindows) $
    case S.viewr oldTiledWindows of
      S.EmptyR -> pure ()
      hs S.:> h ->
        writeIORef
          stateIORef
          state
            { allWorkspacesTiled = BS.insertSeq work (h S.<| hs) (allWorkspacesTiled state)
            }

cycleWindowSlaves :: IORef WMState -> IO ()
cycleWindowSlaves stateIORef = do
  state <- readIORef stateIORef
  let
    work = focusedWorkspace state
    oldTiledWindows = BS.lookupBs work $ allWorkspacesTiled state
  unless (S.null oldTiledWindows) $
    case S.viewr oldTiledWindows of
      S.EmptyR -> pure ()
      hs S.:> h -> case S.viewr hs of
        S.EmptyR -> pure ()
        hss S.:> h2 -> do
          writeIORef
            stateIORef
            state
              { allWorkspacesTiled = BS.insertSeq work ((h2 S.<| hss) S.|> h) (allWorkspacesTiled state)
              }

cycleWindowsBack :: IORef WMState -> IO ()
cycleWindowsBack stateIORef = do
  state <- readIORef stateIORef
  let
    work = focusedWorkspace state
    oldTiledWindows = BS.lookupBs work $ allWorkspacesTiled state
  unless (S.null oldTiledWindows) $
    case S.viewr oldTiledWindows of
      S.EmptyR -> pure ()
      hs S.:> h ->
        writeIORef
          stateIORef
          state
            { allWorkspacesTiled = BS.insertSeq work (h S.<| hs) (allWorkspacesTiled state)
            }

switchWorkspace :: WorkspaceID -> IORef WMState -> IO ()
switchWorkspace workspaceID stateIORef = do
  state <- readIORef stateIORef
  let
    currentFocusedWorkspace = focusedWorkspace state
  unless (workspaceID == currentFocusedWorkspace) $ do
    let
      currentWindowsTiled = BS.lookupBs currentFocusedWorkspace (allWorkspacesTiled state)
      currentWindowsFloating = BS.lookupBs currentFocusedWorkspace (allWorkspacesFloating state)
      newWindowsTiled = BS.lookupBs workspaceID (allWorkspacesTiled state)
      newWindowsFloating = BS.lookupBs workspaceID (allWorkspacesFloating state)

      hidingActions = mapM_ riverWindowHide currentWindowsTiled >> mapM_ riverWindowHide currentWindowsFloating
      showingActions = mapM_ riverWindowShow newWindowsTiled >> mapM_ riverWindowShow newWindowsFloating

      newFocusedWindow
        | S.null newWindowsFloating && S.null newWindowsTiled = Nothing
        | otherwise = focusedWindow state

    writeIORef
      stateIORef
      state
        { renderQueue = renderQueue state >> hidingActions >> showingActions
        , focusedWorkspace = workspaceID
        , focusedWindow = newFocusedWindow
        }
    riverWindowManagerManageDirty (currentWmManager state)

cycleLayout :: [LayoutType] -> IORef WMState -> IO ()
cycleLayout [] _ = pure ()
cycleLayout layouts stateIORef = do
  state <- readIORef stateIORef
  let oldWorkspaceLayouts = workspaceLayouts state
      curr = focusedWorkspace state
      currentLayout = layoutName $ oldWorkspaceLayouts M.! curr
  case elemIndex currentLayout (map layoutName layouts) of
    Nothing -> pure ()
    Just i -> do
      let
        nextLayout = layouts !! ((i + 1) `mod` length layouts)
        newWorkspaceLayouts = M.insert curr nextLayout oldWorkspaceLayouts
      writeIORef stateIORef state{workspaceLayouts = newWorkspaceLayouts}
      riverWindowManagerManageDirty $ currentWmManager state

modifyLayoutRatio :: Int -> IORef WMState -> IO ()
modifyLayoutRatio change stateIORef = do
  state <- readIORef stateIORef
  let newWorkspaceRatios =
        M.insertWith
          (\n o -> let m = n + o in if m > 0 && m < 100 then m else o)
          (focusedWorkspace state)
          change
          (workspaceRatios state)
  writeIORef stateIORef state{workspaceRatios = newWorkspaceRatios}

exec :: String -> IORef WMState -> IO ()
exec command _ = spawnCommand command >> pure ()
