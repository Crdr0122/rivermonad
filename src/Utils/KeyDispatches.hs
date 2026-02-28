module Utils.KeyDispatches where

import Control.Monad (unless, when)
import Data.IORef
import Data.List
import Data.Map qualified as M
import Data.Sequence qualified as S
import Foreign.C
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
          , riverWindowProposeDimensions win w h
          , riverNodeSetPosition
              (nodePtr oldWindow)
              (offsetX + outX o)
              (offsetY + outY o)
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
  case oldTiledWindows of
    S.Empty -> pure ()
    hs S.:|> h ->
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
  case oldTiledWindows of
    S.Empty -> pure ()
    h S.:<| hs -> case hs of
      S.Empty -> pure ()
      h2 S.:<| hss -> do
        writeIORef
          stateIORef
          state
            { allWorkspacesTiled = BS.insertSeq work (h S.<| (hss S.|> h2)) (allWorkspacesTiled state)
            }

cycleWindowsBack :: IORef WMState -> IO ()
cycleWindowsBack stateIORef = do
  state <- readIORef stateIORef
  let
    work = focusedWorkspace state
    oldTiledWindows = BS.lookupBs work $ allWorkspacesTiled state
  case oldTiledWindows of
    S.Empty -> pure ()
    h S.:<| hs ->
      writeIORef
        stateIORef
        state
          { allWorkspacesTiled = BS.insertSeq work (hs S.|> h) (allWorkspacesTiled state)
          }

zoomWindow :: IORef WMState -> IO ()
zoomWindow stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just currentWin -> do
      unless (isFloating (allWindows state M.! currentWin)) $ do
        let
          workspace = focusedWorkspace state
          bimap = allWorkspacesTiled state
          currentSeq = BS.lookupBs workspace bimap
          newSeq = case currentSeq of
            S.Empty -> currentSeq
            w S.:<| ws ->
              if w == currentWin
                then
                  ( case ws of
                      S.Empty -> currentSeq
                      w2 S.:<| wss -> w2 S.<| (w S.<| wss)
                  )
                else case S.elemIndexL currentWin ws of
                  Nothing -> currentSeq
                  Just i -> currentWin S.<| (S.update i w ws)

        let newWorkspacesTiled = BS.insertSeq workspace newSeq bimap
        writeIORef stateIORef state{allWorkspacesTiled = newWorkspacesTiled}
        riverWindowManagerManageDirty (currentWmManager state)

switchWorkspace :: WorkspaceID -> IORef WMState -> IO ()
switchWorkspace workspaceID stateIORef = do
  state <- readIORef stateIORef
  let
    currentFocusedWorkspace = focusedWorkspace state
    (prevWork, nextWork) =
      if (workspaceID == currentFocusedWorkspace)
        then (currentFocusedWorkspace, lastFocusedWorkspace state)
        else (currentFocusedWorkspace, workspaceID)
    currentWindowsTiled = BS.lookupBs prevWork (allWorkspacesTiled state)
    currentWindowsFloating = BS.lookupBs prevWork (allWorkspacesFloating state)
    newWindowsTiled = BS.lookupBs nextWork (allWorkspacesTiled state)
    newWindowsFloating = BS.lookupBs nextWork (allWorkspacesFloating state)

    hidingActions = mapM_ riverWindowHide currentWindowsTiled >> mapM_ riverWindowHide currentWindowsFloating
    showingActions = mapM_ riverWindowShow newWindowsTiled >> mapM_ riverWindowShow newWindowsFloating

    newFocusedWindow = case newWindowsFloating of
      w S.:<| _ -> Just w
      S.Empty -> case newWindowsTiled of
        w S.:<| _ -> Just w
        S.Empty -> Nothing

  writeIORef
    stateIORef
    state
      { renderQueue = renderQueue state >> hidingActions >> showingActions
      , focusedWorkspace = nextWork
      , lastFocusedWorkspace = prevWork
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

modifyLayoutRatio :: CInt -> IORef WMState -> IO ()
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

dragWindow :: IORef WMState -> IO ()
dragWindow stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just w -> when (isFloating (allWindows state M.! w)) $ do
      riverSeatOpStartPointer (focusedSeat state)
      writeIORef stateIORef state{opDeltaState = Dragging}

stopDragging :: IORef WMState -> IO ()
stopDragging stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just w -> do
      let
        window = allWindows state M.! w
        stop = riverSeatOpEnd (focusedSeat state)
      case floatingGeometry window of
        Nothing -> pure ()
        Just r@Rect{rx, ry} -> do
          let
            (dx, dy) = currentOpDelta state
            (newX, newY) = (rx + dx, ry + dy)
            newWindows =
              M.insert w window{floatingGeometry = Just r{rx = newX, ry = newY}} (allWindows state)
          writeIORef
            stateIORef
            state
              { allWindows = newWindows
              , manageQueue = manageQueue state >> stop
              , opDeltaState = None
              , currentOpDelta = (0, 0)
              }

resizeWindow :: IORef WMState -> IO ()
resizeWindow stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just w -> when (isFloating (allWindows state M.! w)) $ do
      let warp = case (floatingGeometry (allWindows state M.! w)) of
            Nothing -> pure ()
            Just Rect{rx, ry, rw, rh} -> do
              let
                Output{outX, outY} = allOutputs state M.! focusedOutput state
              riverSeatPointerWarp (focusedSeat state) (outX + rw + rx) (outY + rh + ry)
      writeIORef
        stateIORef
        state
          { opDeltaState = Resizing
          , manageQueue = manageQueue state >> riverWindowInformResizeStart w
          }
      riverSeatOpStartPointer (focusedSeat state)

stopResizing :: IORef WMState -> IO ()
stopResizing stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just w -> do
      let
        window = allWindows state M.! w
        stop = riverSeatOpEnd (focusedSeat state)
      case floatingGeometry window of
        Nothing -> pure ()
        Just r@Rect{rw, rh} -> do
          let
            (dx, dy) = currentOpDelta state
            (newW, newH) = (max (rw + dx) 15, max (rh + dy) 15)
            newWindows =
              M.insert w window{floatingGeometry = Just r{rw = newW, rh = newH}} (allWindows state)
          writeIORef
            stateIORef
            state
              { allWindows = newWindows
              , manageQueue = manageQueue state >> stop >> riverWindowInformResizeEnd w
              , opDeltaState = None
              , currentOpDelta = (0, 0)
              }
