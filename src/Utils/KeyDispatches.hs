module Utils.KeyDispatches (
  closeCurrentWindow,
  toggleFullscreenCurrentWindow,
  toggleFloatingCurrentWindow,
  cycleWindows,
  cycleWindowSlaves,
  cycleLayout,
  zoomWindow,
  exec,
  resizeWindow,
  stopResizing,
  dragWindow,
  stopDragging,
  modifyLayoutRatio,
  moveWindowToWorkspace,
  switchWorkspace,
) where

import Control.Monad (unless, when)
import Data.IORef
import Data.List
import Data.Map qualified as M
import Data.Sequence qualified as S
import System.Process
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.ImportedFunctions

closeCurrentWindow :: IORef WMState -> IO ()
closeCurrentWindow stateIORef = do
  state <- readIORef stateIORef
  let closeWindow (Just w) = riverWindowClose w
      closeWindow Nothing = pure ()
  writeIORef stateIORef state{manageQueue = manageQueue state >> closeWindow (focusedWindow state)}

toggleFullscreenCurrentWindow :: IORef WMState -> IO ()
toggleFullscreenCurrentWindow stateIORef = do
  state@WMState
    { focusedWindow
    , allWindows
    , allWorkspacesFloating
    , allWorkspacesFullscreen
    , allWorkspacesTiled
    , fullscreenQueue
    , floatingQueue
    , focusedWorkspace
    , focusedOutput
    , currentWmManager
    } <-
    readIORef stateIORef
  case focusedWindow of
    Nothing -> pure ()
    Just win -> do
      let window@Window{isFloating, isFullscreen} = allWindows M.! win
          newWindows = M.insert win window{isFullscreen = not isFullscreen} allWindows
      let newState = if isFullscreen then exitFullscreenWindow isFloating else fullscreenWindow isFloating
      writeIORef stateIORef newState{allWindows = newWindows}
      riverWindowManagerManageDirty currentWmManager
     where
      fullscreenWindow f = do
        let fA = riverWindowFullscreen win focusedOutput
        if f
          then
            state
              { allWorkspacesFloating = BS.delete win allWorkspacesFloating
              , fullscreenQueue = win : fullscreenQueue
              , manageQueue = manageQueue state >> fA
              }
          else
            state
              { allWorkspacesTiled = BS.delete win allWorkspacesTiled
              , fullscreenQueue = win : fullscreenQueue
              , manageQueue = manageQueue state >> fA
              }

      exitFullscreenWindow f = do
        let fA = riverWindowExitFullscreen win
        if f
          then
            state
              { allWorkspacesFullscreen = BS.delete win allWorkspacesFullscreen
              , floatingQueue = win : floatingQueue
              , manageQueue = manageQueue state >> fA
              }
          else
            state
              { allWorkspacesFullscreen = BS.delete win allWorkspacesFullscreen
              , allWorkspacesTiled = BS.insert focusedWorkspace win allWorkspacesTiled
              , manageQueue = manageQueue state >> fA
              }

toggleFloatingCurrentWindow :: IORef WMState -> IO ()
toggleFloatingCurrentWindow stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just win -> do
      unless (isFullscreen (allWindows state M.! win)) $
        if isFloating (allWindows state M.! win) then tileCurrentWindow stateIORef else floatCurrentWindow stateIORef

floatCurrentWindow :: IORef WMState -> IO ()
floatCurrentWindow stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just winP -> do
      let
        o = allOutputs state M.! focusedOutput state
        window = (allWindows state M.! winP)
        (calcPos, mAction, rAction) = calculateFloatingPosition winP window o
        newWindows =
          M.adjust
            (\w -> w{isFloating = True, floatingGeometry = Just calcPos})
            winP
            (allWindows state)
        newTiled = BS.delete winP (allWorkspacesTiled state)
        newFloating = BS.insert (focusedWorkspace state) winP (allWorkspacesFloating state)

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

cycleWindows :: Bool -> IORef WMState -> IO ()
cycleWindows forward stateIORef = do
  state <- readIORef stateIORef
  let
    work = focusedWorkspace state
    oldTiledWindows = BS.lookupBs work $ allWorkspacesTiled state
    cycleW _ S.Empty = S.empty
    cycleW True (h S.:<| hs) = hs S.|> h
    cycleW False (hs S.:|> h) = h S.<| hs
  case oldTiledWindows of
    S.Empty -> pure ()
    s ->
      writeIORef
        stateIORef
        state
          { allWorkspacesTiled = BS.insertSeq work (cycleW forward s) (allWorkspacesTiled state)
          }

cycleWindowSlaves :: Bool -> IORef WMState -> IO ()
cycleWindowSlaves forward stateIORef = do
  state <- readIORef stateIORef
  let
    work = focusedWorkspace state
    oldTiledWindows = BS.lookupBs work $ allWorkspacesTiled state
    cycleW _ S.Empty = S.empty
    cycleW True (hs S.:|> h) = h S.<| hs
    cycleW False (h S.:<| hs) = hs S.|> h
  case oldTiledWindows of
    S.Empty -> pure ()
    h S.:<| hs -> case hs of
      S.Empty -> pure ()
      s -> do
        writeIORef
          stateIORef
          state
            { allWorkspacesTiled = BS.insertSeq work (h S.<| (cycleW forward s)) (allWorkspacesTiled state)
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

moveWindowToWorkspace :: WorkspaceID -> Bool -> IORef WMState -> IO ()
moveWindowToWorkspace targetID silent stateIORef = do
  state@WMState
    { allWorkspacesFloating
    , allWorkspacesTiled
    , focusedWorkspace
    , allWindows
    , currentWmManager
    , renderQueue
    } <-
    readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just w -> do
      let
        Window{isFloating} = allWindows M.! w
      unless (focusedWorkspace == targetID) $ do
        let
          (newAllTile, newAllFloat) =
            if isFloating
              then
                ( allWorkspacesTiled
                , BS.insert targetID w (BS.delete w allWorkspacesFloating)
                )
              else
                ( BS.insert targetID w (BS.delete w allWorkspacesTiled)
                , allWorkspacesFloating
                )
        writeIORef
          stateIORef
          state
            { allWorkspacesTiled = newAllTile
            , allWorkspacesFloating = newAllFloat
            , renderQueue = renderQueue >> riverWindowHide w
            }
        if silent
          then riverWindowManagerManageDirty currentWmManager
          else switchWorkspace targetID stateIORef

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

modifyLayoutRatio :: Double -> IORef WMState -> IO ()
modifyLayoutRatio change stateIORef = do
  state <- readIORef stateIORef
  let
    newWorkspaceRatios =
      M.insertWith
        (\n o -> let m = n + o in if m > 0.20 && m < 0.80 then m else o)
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
        Just r -> do
          let
            (x, y, _, _) = currentOpDelta state
            newWindows =
              M.insert w window{floatingGeometry = Just r{rx = x, ry = y}} (allWindows state)
          writeIORef
            stateIORef
            state
              { allWindows = newWindows
              , manageQueue = manageQueue state >> stop
              , opDeltaState = None
              , currentOpDelta = (0, 0, 0, 0)
              }

resizeWindow :: IORef WMState -> IO ()
resizeWindow stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just w -> do
      let
        win = (allWindows state M.! w)
      if isFloating win
        then do
          case floatingGeometry win of
            Nothing -> pure ()
            Just Rect{rx, ry, rw, rh} -> do
              let (cX, cY) = cursorPosition state
                  edge
                    | cX < firstX && cY < firstY = edgeTopLeft
                    | cX < secondX && cY < firstY = edgeTop
                    | cY < firstY = edgeTopRight
                    | cX < firstX && cY < secondY = edgeLeft
                    | cX < oneHalfX && cY < oneHalfY = edgeTopLeft
                    | cX < secondX && cY < oneHalfY = edgeTopRight
                    | cX < oneHalfX && cY < secondY = edgeBottomLeft
                    | cX < secondX && cY < secondY = edgeBottomRight
                    | cY < secondY = edgeRight
                    | cX < firstX = edgeBottomLeft
                    | cX < secondX = edgeBottom
                    | otherwise = edgeBottomRight
                   where
                    oneThirdW = rw `div` 3
                    oneThirdH = rh `div` 3
                    oneHalfX = rx + rw `div` 2
                    oneHalfY = ry + rh `div` 2
                    firstX = rx + oneThirdW
                    secondX = firstX + oneThirdW
                    firstY = ry + oneThirdH
                    secondY = firstY + oneThirdH

              writeIORef
                stateIORef
                state
                  { opDeltaState = Resizing edge
                  , manageQueue = manageQueue state >> riverWindowInformResizeStart w
                  }
              riverSeatOpStartPointer (focusedSeat state)
        else do
          writeIORef
            stateIORef
            state
              { opDeltaState = ResizingTile
              , manageQueue = manageQueue state >> riverWindowInformResizeStart w
              }
          riverSeatOpStartPointer (focusedSeat state)

stopResizing :: IORef WMState -> IO ()
stopResizing stateIORef = do
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just win -> do
      let
        window = allWindows state M.! win
        stop = riverSeatOpEnd (focusedSeat state)
      case floatingGeometry window of
        Nothing -> pure ()
        Just r -> do
          let (x, y, w, h) = currentOpDelta state
              newWindows =
                M.insert
                  win
                  window{floatingGeometry = Just r{rw = w, rh = h, rx = x, ry = y}}
                  (allWindows state)
          writeIORef stateIORef state{allWindows = newWindows}
      writeIORef
        stateIORef
        state
          { manageQueue = manageQueue state >> stop >> riverWindowInformResizeEnd win
          , opDeltaState = None
          , currentOpDelta = (0, 0, 0, 0)
          }
