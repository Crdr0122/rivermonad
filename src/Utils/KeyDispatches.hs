{-# LANGUAGE MultiWayIf #-}

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
  doNothing,
  toggleFocusFloating,
  togglePinWindow,
  cycleWindowFocus,
  reloadWindowManager,
) where

import Control.Concurrent.MVar
import Control.Monad (unless)
import Data.Aeson
import Data.Bimap qualified as B
import Data.List (elemIndex)
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as S
import System.Process
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.ImportedFunctions

doNothing :: MVar WMState -> IO ()
doNothing _ = pure ()

closeCurrentWindow :: MVar WMState -> IO ()
closeCurrentWindow stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    let closeWindow (Just w) = riverWindowClose w
        closeWindow Nothing = pure ()
    pure $ state{manageQueue = manageQueue state >> closeWindow (focusedWindow state)}

toggleFocusFloating :: MVar WMState -> IO ()
toggleFocusFloating stateMVar = do
  modifyMVar_ stateMVar $ \state@WMState{allOutputWorkspaces, focusedOutput, focusedWindow} -> do
    case focusedWindow of
      Nothing -> pure state
      Just w -> do
        let window = allWindows state M.! w
            focusedWorkspace = allOutputWorkspaces B.! focusedOutput
        if
          | isFullscreen window -> pure state
          | isFloating window -> do
              case BS.lookupBs focusedWorkspace (allWorkspacesTiled state) of
                S.Empty -> pure state
                h S.:<| _ -> pure state{focusedWindow = Just h}
          | otherwise -> do
              case BS.lookupBs focusedWorkspace (allWorkspacesFloating state) of
                S.Empty -> pure state
                h S.:<| _ -> pure state{focusedWindow = Just h}

cycleWindowFocus :: Bool -> MVar WMState -> IO ()
cycleWindowFocus forward stateMVar = do
  modifyMVar_ stateMVar $ \state@WMState{allOutputWorkspaces, focusedOutput, focusedWindow} -> do
    case focusedWindow of
      Nothing -> pure state
      Just w -> do
        let Window{isFullscreen, isFloating} = allWindows state M.! w
            focusedWorkspace = allOutputWorkspaces B.! focusedOutput
            nextWindow bm = BS.lookUpNext focusedWorkspace forward w bm
        if
          | isFullscreen -> do
              let
                next = nextWindow (allWorkspacesFullscreen state)
                Window{nodePtr} = allWindows state M.! next
              pure
                state
                  { focusedWindow = Just $ next
                  , renderQueue = renderQueue state >> riverNodePlaceTop nodePtr
                  }
          | isFloating -> do
              let
                next = nextWindow (allWorkspacesFloating state)
                Window{nodePtr} = allWindows state M.! next
              pure
                state
                  { focusedWindow = Just $ next
                  , renderQueue = renderQueue state >> riverNodePlaceTop nodePtr
                  }
          | otherwise -> do
              let
                next = nextWindow (allWorkspacesTiled state)
              pure
                state
                  { focusedWindow = Just $ next
                  , manageQueue = manageQueue state
                  }

toggleFullscreenCurrentWindow :: MVar WMState -> IO ()
toggleFullscreenCurrentWindow stateMVar = do
  modifyMVar_ stateMVar $
    \state@WMState
       { focusedWindow
       , allWindows
       , allWorkspacesFloating
       , allWorkspacesFullscreen
       , allWorkspacesTiled
       , floatingQueue
       , fullscreenQueue
       , allOutputWorkspaces
       , focusedOutput
       , currentWmManager
       } -> do
        case focusedWindow of
          Nothing -> pure state
          Just win -> do
            let window@Window{isFloating, isFullscreen, isPinned} = allWindows M.! win
            if isPinned
              then pure state
              else do
                let focusedWorkspace = allOutputWorkspaces B.! focusedOutput

                    fullscreenWindow f
                      | f =
                          state
                            { allWorkspacesFloating = BS.delete win allWorkspacesFloating
                            , fullscreenQueue = M.adjust (win :) focusedWorkspace fullscreenQueue
                            }
                      | otherwise =
                          state
                            { allWorkspacesTiled = BS.delete win allWorkspacesTiled
                            , fullscreenQueue = M.adjust (win :) focusedWorkspace fullscreenQueue
                            }

                    exitFullscreenWindow f
                      | f =
                          state
                            { allWorkspacesFullscreen = BS.delete win allWorkspacesFullscreen
                            , floatingQueue = M.adjust (win :) focusedWorkspace floatingQueue
                            , manageQueue = manageQueue state >> fA
                            }
                      | otherwise =
                          state
                            { allWorkspacesFullscreen = BS.delete win allWorkspacesFullscreen
                            , allWorkspacesTiled = BS.insert focusedWorkspace win allWorkspacesTiled
                            , manageQueue = manageQueue state >> fA
                            }
                     where
                      fA = riverWindowExitFullscreen win >> riverWindowInformNotFullscreen win

                    newState = if isFullscreen then exitFullscreenWindow isFloating else fullscreenWindow isFloating
                    newWindows = M.insert win window{isFullscreen = not isFullscreen} allWindows

                riverWindowManagerManageDirty currentWmManager
                pure newState{allWindows = newWindows}

toggleFloatingCurrentWindow :: MVar WMState -> IO ()
toggleFloatingCurrentWindow stateMVar = do
  state <- readMVar stateMVar
  case focusedWindow state of
    Nothing -> pure ()
    Just win -> do
      let w = allWindows state M.! win
      unless (isFullscreen w || isPinned w) $
        if isFloating w then tileCurrentWindow stateMVar else floatCurrentWindow stateMVar

floatCurrentWindow :: MVar WMState -> IO ()
floatCurrentWindow stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Nothing -> pure state
      Just win -> do
        let newTiled = BS.delete win (allWorkspacesTiled state)
            focusedWorkspace = allOutputWorkspaces state B.! focusedOutput state
        pure $ state{allWorkspacesTiled = newTiled, floatingQueue = M.adjust (win :) focusedWorkspace (floatingQueue state)}

tileCurrentWindow :: MVar WMState -> IO ()
tileCurrentWindow stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Nothing -> pure state
      Just win -> do
        let
          newFloating = BS.delete win (allWorkspacesFloating state)
          newTiled = BS.insert (allOutputWorkspaces state B.! focusedOutput state) win (allWorkspacesTiled state)
          newAllWindows = M.adjust (\w -> w{isFloating = False}) win (allWindows state)
        pure $ state{allWorkspacesFloating = newFloating, allWorkspacesTiled = newTiled, allWindows = newAllWindows}

togglePinWindow :: MVar WMState -> IO ()
togglePinWindow stateMVar = do
  modifyMVar_ stateMVar $ \state ->
    case focusedWindow state of
      Nothing -> pure state
      Just win -> do
        let
          Window{isFullscreen, isFloating} = allWindows state M.! win
        if isFloating && not isFullscreen
          then do
            let newWindows = M.adjust (\w -> w{isPinned = not (isPinned w)}) win (allWindows state)
            pure state{allWindows = newWindows}
          else pure state

cycleWindows :: Bool -> MVar WMState -> IO ()
cycleWindows forward stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    let
      work = allOutputWorkspaces state B.! focusedOutput state
      cycleW _ S.Empty = S.empty
      cycleW True (h S.:<| hs) = hs S.|> h
      cycleW False (hs S.:|> h) = h S.<| hs
    pure
      state
        { allWorkspacesTiled = BS.changeSeqOrder work (cycleW forward) (allWorkspacesTiled state)
        }

cycleWindowSlaves :: Bool -> MVar WMState -> IO ()
cycleWindowSlaves forward stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    let
      work = allOutputWorkspaces state B.! focusedOutput state
      cycleW True (h S.:<| (hs S.:|> slaveH)) = h S.<| (slaveH S.<| hs)
      cycleW False (h S.:<| (slaveH S.:<| hs)) = h S.<| (hs S.|> slaveH)
      cycleW _ hs = hs
    pure $
      state
        { allWorkspacesTiled = BS.changeSeqOrder work (cycleW forward) (allWorkspacesTiled state)
        }

zoomWindow :: MVar WMState -> IO ()
zoomWindow stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Just currentWin ->
        if (\w -> isFloating w || isFullscreen w) (allWindows state M.! currentWin)
          then pure state
          else do
            let
              workspace = allOutputWorkspaces state B.! focusedOutput state
              bimap = allWorkspacesTiled state
              zoom s = case s of
                S.Empty -> s
                w S.:<| ws ->
                  if w == currentWin
                    then
                      ( case ws of
                          S.Empty -> s
                          w2 S.:<| wss -> w2 S.<| (w S.<| wss)
                      )
                    else case S.elemIndexL currentWin ws of
                      Nothing -> s
                      Just i -> currentWin S.<| (S.update i w ws)

            let newWorkspacesTiled = BS.changeSeqOrder workspace zoom bimap
            riverWindowManagerManageDirty (currentWmManager state)
            pure $ state{allWorkspacesTiled = newWorkspacesTiled}
      Nothing -> pure state

switchWorkspace :: WorkspaceID -> MVar WMState -> IO ()
switchWorkspace targetID stateMVar = do
  nextAction <-
    modifyMVar stateMVar $
      \state@WMState
         { allOutputWorkspaces
         , focusedOutput
         , allWindows
         , allWorkspacesFloating
         , lastFocusedWorkspace
         , focusedSeat
         } -> do
          let
            currentFocusedWorkspace = allOutputWorkspaces B.! focusedOutput
          if
            | currentFocusedWorkspace == targetID && lastFocusedWorkspace == targetID -> pure (state, pure ())
            | currentFocusedWorkspace == targetID -> pure (state, switchWorkspace lastFocusedWorkspace stateMVar)
            | otherwise -> do
                let
                  alreadyShowing = B.lookupR targetID allOutputWorkspaces
                  currentWindows = allWorkspaceWindows currentFocusedWorkspace state
                  newWindows = allWorkspaceWindows targetID state
                  newOutput = B.insert focusedOutput targetID allOutputWorkspaces
                  pinnedWindows = S.filter (\w -> isPinned $ allWindows M.! w) $ BS.lookupBs currentFocusedWorkspace allWorkspacesFloating
                  newWorkspacesFloating = foldl' (\bimap w -> BS.move w targetID bimap) allWorkspacesFloating pinnedWindows

                  (newOutputWorkspaces, hidingActions, showingActions) = case alreadyShowing of
                    Nothing ->
                      ( newOutput
                      , mapM_ (\w -> unless (isPinned $ allWindows M.! w) $ riverWindowHide w) currentWindows
                      , mapM_ riverWindowShow newWindows
                      )
                    Just o2 ->
                      ( B.insert o2 currentFocusedWorkspace newOutput
                      , pure ()
                      , pure ()
                      )

                  (newFocusedWindow, focusAction) = case newWindows S.>< pinnedWindows of
                    w S.:<| _ -> (Just w, riverSeatFocusWindow focusedSeat w)
                    S.Empty -> (Nothing, pure ())

                print $ "Pinned: " ++ show pinnedWindows

                pure
                  ( state
                      { renderQueue = renderQueue state >> hidingActions >> showingActions
                      , manageQueue = manageQueue state >> focusAction
                      , allOutputWorkspaces = newOutputWorkspaces
                      , lastFocusedWorkspace = currentFocusedWorkspace
                      , allWorkspacesFloating = newWorkspacesFloating
                      , focusedWindow = newFocusedWindow
                      }
                  , riverWindowManagerManageDirty (currentWmManager state)
                  )
  nextAction

moveWindowToWorkspace :: WorkspaceID -> MVar WMState -> IO ()
moveWindowToWorkspace targetID stateMVar = do
  modifyMVar_ stateMVar $
    \state@WMState
       { allWorkspacesFloating
       , allWorkspacesTiled
       , allWorkspacesFullscreen
       , allWindows
       , focusedOutput
       , allOutputWorkspaces
       , currentWmManager
       , renderQueue
       } -> do
        case focusedWindow state of
          Nothing -> pure state
          Just w -> do
            let
              Window{isFloating, isPinned, isFullscreen} = allWindows M.! w
            if allOutputWorkspaces B.! focusedOutput == targetID || isPinned
              then pure state
              else do
                let
                  newState
                    | isFullscreen = state{allWorkspacesFullscreen = BS.move w targetID allWorkspacesFullscreen}
                    | isFloating = state{allWorkspacesFloating = BS.move w targetID allWorkspacesFloating}
                    | otherwise = state{allWorkspacesTiled = BS.move w targetID allWorkspacesTiled}
                riverWindowManagerManageDirty currentWmManager
                pure newState{renderQueue = renderQueue >> riverWindowHide w}

cycleLayout :: [LayoutType] -> MVar WMState -> IO ()
cycleLayout [] _ = pure ()
cycleLayout layouts stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    let oldWorkspaceLayouts = workspaceLayouts state
        curr = allOutputWorkspaces state B.! focusedOutput state
        currentLayout = layoutName $ oldWorkspaceLayouts M.! curr
    case elemIndex currentLayout (map layoutName layouts) of
      Nothing -> pure state
      Just i -> do
        let
          nextLayout = layouts !! ((i + 1) `mod` length layouts)
          newWorkspaceLayouts = M.insert curr nextLayout oldWorkspaceLayouts
        riverWindowManagerManageDirty $ currentWmManager state
        pure state{workspaceLayouts = newWorkspaceLayouts}

modifyLayoutRatio :: Double -> MVar WMState -> IO ()
modifyLayoutRatio change stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    let
      newWorkspaceRatios =
        M.insertWith
          (\n o -> let m = n + o in if m > 0.20 && m < 0.80 then m else o)
          (allOutputWorkspaces state B.! focusedOutput state)
          change
          (workspaceRatios state)
    pure state{workspaceRatios = newWorkspaceRatios}

exec :: String -> MVar WMState -> IO ()
exec command _ = spawnCommand ("systemd-run --user --scope --slice=app.slice " ++ command) >> pure ()

reloadWindowManager :: FilePath -> MVar WMState -> IO ()
reloadWindowManager fp stateMVar = do
  modifyMVar_ stateMVar $
    \state@WMState
       { allWorkspacesTiled
       , allWorkspacesFloating
       , allWorkspacesFullscreen
       , allWindows
       , workspaceRatios
       } -> do
        let
          windowsToRecord =
            M.fromList $
              fmap
                ( \Window{winPtr, winIdentifier, isFloating, isFullscreen} ->
                    if
                      | isFloating && isFullscreen -> (winIdentifier, (fromMaybe 1 (BS.lookupA winPtr allWorkspacesFullscreen), FullscreenFloating))
                      | isFullscreen -> (winIdentifier, (fromMaybe 1 (BS.lookupA winPtr allWorkspacesFullscreen), Fullscreen))
                      | isFloating -> (winIdentifier, (fromMaybe 1 (BS.lookupA winPtr allWorkspacesFloating), Floating))
                      | otherwise -> (winIdentifier, (fromMaybe 1 (BS.lookupA winPtr allWorkspacesTiled), Tiled))
                )
                (M.elems allWindows)
          newPersisted = PersistedState{persistedWorkspaceRatios = workspaceRatios, persistedWindows = windowsToRecord}
        encodeFile fp newPersisted
        _ <- spawnCommand ("systemd-run --user --scope --slice=app.slice Rivermonad-reload")
        pure state

dragWindow :: MVar WMState -> IO ()
dragWindow stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Nothing -> pure state
      Just w -> do
        let win = allWindows state M.! w
        if
          | isFullscreen win -> pure state
          | isFloating win -> do
              riverSeatOpStartPointer (focusedSeat state)
              pure state{opDeltaState = Dragging}
          | otherwise -> do
              let Rect{rx, ry} = fromMaybe (Rect 0 0 0 0) $ tilingGeometry win
                  newTiles = BS.delete w (allWorkspacesTiled state)
              riverSeatOpStartPointer (focusedSeat state)
              pure state{opDeltaState = DraggingTile w, currentOpDelta = (rx, ry, 0, 0), allWorkspacesTiled = newTiles}

stopDragging :: MVar WMState -> IO ()
stopDragging stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case opDeltaState state of
      Dragging ->
        case focusedWindow state of
          Nothing -> pure state
          Just w -> do
            let
              window = allWindows state M.! w
              stop = riverSeatOpEnd (focusedSeat state)
            case floatingGeometry window of
              Nothing -> pure state
              Just r -> do
                let
                  (x, y, _, _) = currentOpDelta state
                  newWindows =
                    M.insert w window{floatingGeometry = Just r{rx = x, ry = y}} (allWindows state)
                pure
                  state
                    { allWindows = newWindows
                    , manageQueue = manageQueue state >> stop
                    , opDeltaState = None
                    , currentOpDelta = (0, 0, 0, 0)
                    }
      DraggingTile w -> do
        let
          focusedWorkspace = allOutputWorkspaces state B.! focusedOutput state
          (currentX, currentY, _, _) = currentOpDelta state
          currentTiled = BS.lookupBs focusedWorkspace (allWorkspacesTiled state)
          calcDistance (x1, y1) (x2, y2) = sqrt (fromIntegral $ (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int))
          distances :: S.Seq Double
          distances =
            ((\Rect{rx, ry} -> calcDistance (rx, ry) (currentX, currentY)) . (fromMaybe (Rect 0 0 0 0)) . tilingGeometry . (allWindows state M.!))
              <$> currentTiled
          index =
            case distances of
              S.Empty -> 0
              h S.:<| t -> fst $ S.foldlWithIndex (\(oldI, old) i new -> if new < old then (i + 1, new) else (oldI, old)) (0, h) t
          newTiled = BS.insertByIndex focusedWorkspace w (fromIntegral index) (allWorkspacesTiled state)
          stop = riverSeatOpEnd (focusedSeat state)
        pure
          state
            { manageQueue = manageQueue state >> stop
            , opDeltaState = None
            , allWorkspacesTiled = newTiled
            , currentOpDelta = (0, 0, 0, 0)
            }
      _ -> pure state

resizeWindow :: MVar WMState -> IO ()
resizeWindow stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Nothing -> pure state
      Just w -> do
        let
          win@Window{isFullscreen, isFloating} = (allWindows state M.! w)
        if
          | isFullscreen -> pure state
          | isFloating -> do
              case floatingGeometry win of
                Nothing -> pure state
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

                  riverSeatOpStartPointer (focusedSeat state)
                  pure
                    state
                      { opDeltaState = Resizing edge
                      , manageQueue = manageQueue state >> riverWindowInformResizeStart w
                      }
          | otherwise -> do
              riverSeatOpStartPointer (focusedSeat state)
              pure
                state
                  { opDeltaState = ResizingTile
                  , manageQueue = manageQueue state >> riverWindowInformResizeStart w
                  }

stopResizing :: MVar WMState -> IO ()
stopResizing stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Nothing -> pure state
      Just win -> do
        let
          window = allWindows state M.! win
          stop = riverSeatOpEnd (focusedSeat state)
          newState = case (opDeltaState state, floatingGeometry window) of
            (Resizing _, Just r) -> do
              let (x, y, w, h) = currentOpDelta state
                  newWindows =
                    M.insert
                      win
                      window{floatingGeometry = Just r{rw = w, rh = h, rx = x, ry = y}}
                      (allWindows state)
              state{allWindows = newWindows}
            _ -> state
        pure
          newState
            { manageQueue = manageQueue state >> stop >> riverWindowInformResizeEnd win
            , opDeltaState = None
            , currentOpDelta = (0, 0, 0, 0)
            }
