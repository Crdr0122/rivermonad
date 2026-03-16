{-# LANGUAGE MultiWayIf #-}

module Utils.KeyDispatches (
  closeCurrentWindow,
  toggleFullscreenCurrentWindow,
  toggleFloatingCurrentWindow,
  cycleWindows,
  cycleWindowSlaves,
  sendMessage,
  zoomWindow,
  exec,
  resizeWindow,
  stopResizing,
  dragWindow,
  stopDragging,
  moveWindowToWorkspace,
  switchWorkspace,
  doNothing,
  toggleFocusFloating,
  togglePinWindow,
  toggleMaximizeWindow,
  cycleWindowFocus,
  focusWindow,
  swapWindow,
  reloadWindowManager,
  exitSession,
  startRepeating,
  stopRepeating,
) where

import Control.Concurrent
import Control.Monad (forever, unless)
import Data.Aeson
import Data.Bimap qualified as B
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as S
import Foreign
import System.IO
import System.Process
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.ImportedFunctions

doNothing :: Ptr RiverSeat -> MVar WMState -> IO ()
doNothing _ _ = pure ()

sendMessage :: LayoutMsg -> Ptr RiverSeat -> MVar WMState -> IO ()
sendMessage msg _ stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    let focusedWorkspace = allOutputWorkspaces state B.! focusedOutput state
    case handleSomeMsg (workspaceLayouts state M.! focusedWorkspace) msg of
      Nothing -> pure state
      Just layout -> do
        riverWindowManagerManageDirty (currentWmManager state)
        pure state{workspaceLayouts = M.insert focusedWorkspace layout (workspaceLayouts state)}

exitSession :: Ptr RiverSeat -> MVar WMState -> IO ()
exitSession _ stateMVar =
  modifyMVar_ stateMVar $ \state -> do
    riverWindowManagerExitSession (currentWmManager state)
    pure state

closeCurrentWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
closeCurrentWindow _ stateMVar = do
  modifyMVar_ stateMVar $ \state ->
    case focusedWindow state of
      Nothing -> pure state
      Just w ->
        pure
          state
            { manageQueue = manageQueue state >> riverWindowClose w
            }

toggleFocusFloating :: Ptr RiverSeat -> MVar WMState -> IO ()
toggleFocusFloating _ stateMVar = do
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

cycleWindowFocus :: Bool -> Ptr RiverSeat -> MVar WMState -> IO ()
cycleWindowFocus forward seat stateMVar = do
  modifyMVar_ stateMVar $ \state@WMState{allOutputWorkspaces, focusedOutput, focusedWindow} -> do
    case focusedWindow of
      Nothing -> pure state
      Just w ->
        do
          let Window{isFullscreen, isFloating} = allWindows state M.! w
              focusedWorkspace = allOutputWorkspaces B.! focusedOutput
              nextWindow bm = BS.lookUpNext focusedWorkspace forward w bm
              (next, renderAction)
                | isFullscreen =
                    let Window{nodePtr} = allWindows state M.! next
                     in (nextWindow (allWorkspacesFullscreen state), riverNodePlaceTop nodePtr)
                | isFloating =
                    let Window{nodePtr} = allWindows state M.! next
                     in (nextWindow (allWorkspacesFloating state), riverNodePlaceTop nodePtr)
                | otherwise = (nextWindow (allWorkspacesTiled state), pure ())
          pure
            state
              { focusedWindow = Just next
              , renderQueue = renderQueue state >> renderAction
              , manageQueue = manageQueue state >> riverSeatFocusWindow seat next
              }

toggleFullscreenCurrentWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
toggleFullscreenCurrentWindow _ stateMVar = do
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

toggleFloatingCurrentWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
toggleFloatingCurrentWindow seat stateMVar = do
  state <- readMVar stateMVar
  case focusedWindow state of
    Nothing -> pure ()
    Just win -> do
      let w = allWindows state M.! win
      unless (isFullscreen w || isPinned w) $
        if isFloating w then tileCurrentWindow seat stateMVar else floatCurrentWindow seat stateMVar

floatCurrentWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
floatCurrentWindow _ stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Nothing -> pure state
      Just win -> do
        let newTiled = BS.delete win (allWorkspacesTiled state)
            focusedWorkspace = allOutputWorkspaces state B.! focusedOutput state
        pure $ state{allWorkspacesTiled = newTiled, floatingQueue = M.adjust (win :) focusedWorkspace (floatingQueue state)}

tileCurrentWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
tileCurrentWindow _ stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Nothing -> pure state
      Just win -> do
        let
          newFloating = BS.delete win (allWorkspacesFloating state)
          newTiled = BS.insert (allOutputWorkspaces state B.! focusedOutput state) win (allWorkspacesTiled state)
          newAllWindows = M.adjust (\w -> w{isFloating = False}) win (allWindows state)
        pure $ state{allWorkspacesFloating = newFloating, allWorkspacesTiled = newTiled, allWindows = newAllWindows}

togglePinWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
togglePinWindow _ stateMVar = do
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

toggleMaximizeWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
toggleMaximizeWindow _ stateMVar = do
  modifyMVar_ stateMVar $ \state ->
    case focusedWindow state of
      Nothing -> pure state
      Just win -> do
        let Window{isMaximized} = allWindows state M.! win
            newWindows = M.adjust (\w -> w{isMaximized = not isMaximized}) win (allWindows state)
            maximizeAction = if isMaximized then riverWindowInformUnmaximized win else riverWindowInformMaximized win
        pure state{allWindows = newWindows, manageQueue = manageQueue state >> maximizeAction}

cycleWindows :: Bool -> Ptr RiverSeat -> MVar WMState -> IO ()
cycleWindows forward seat stateMVar = do
  modifyMVar_ stateMVar $
    \state@WMState
       { allOutputWorkspaces
       , focusedOutput
       , allWorkspacesTiled
       , focusedWindow
       , manageQueue
       } -> do
        let
          work = allOutputWorkspaces B.! focusedOutput
          cycleW _ S.Empty = S.empty
          cycleW True (h S.:<| hs) = hs S.|> h
          cycleW False (hs S.:|> h) = h S.<| hs
          (nextFocus, focusAction) = case focusedWindow of
            Nothing -> (focusedWindow, pure ())
            Just w -> case BS.lookupA w allWorkspacesTiled of
              Nothing -> (focusedWindow, pure ())
              Just workspace -> do
                let win = BS.lookUpNext workspace forward w allWorkspacesTiled
                (Just win, riverSeatFocusWindow seat win)
        pure
          state
            { allWorkspacesTiled = BS.changeSeqOrder work (cycleW forward) allWorkspacesTiled
            , focusedWindow = nextFocus
            , manageQueue = manageQueue >> focusAction
            }

cycleWindowSlaves :: Bool -> Ptr RiverSeat -> MVar WMState -> IO ()
cycleWindowSlaves forward seat stateMVar = do
  modifyMVar_ stateMVar $
    \state@WMState
       { allOutputWorkspaces
       , focusedOutput
       , allWorkspacesTiled
       , focusedWindow
       , manageQueue
       } -> do
        let
          work = allOutputWorkspaces B.! focusedOutput
          cycleW True (h S.:<| (hs S.:|> slaveH)) = h S.<| (slaveH S.<| hs)
          cycleW False (h S.:<| (slaveH S.:<| hs)) = h S.<| (hs S.|> slaveH)
          cycleW _ hs = hs
          (nextFocus, focusAction) = case focusedWindow of
            Nothing -> (focusedWindow, pure ())
            Just w -> do
              let s = BS.lookupBs work allWorkspacesTiled
              case S.elemIndexL w s of
                Just i
                  | i /= 0 ->
                      let nextW = S.index s (((if forward then i else i - 2) `mod` (length s - 1)) + 1)
                       in (Just nextW, riverSeatFocusWindow seat nextW)
                _ -> (focusedWindow, pure ())
        pure $
          state
            { allWorkspacesTiled = BS.changeSeqOrder work (cycleW forward) allWorkspacesTiled
            , focusedWindow = nextFocus
            , manageQueue = manageQueue >> focusAction
            }

zoomWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
zoomWindow _ stateMVar = do
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

switchWorkspace :: WorkspaceID -> Ptr RiverSeat -> MVar WMState -> IO ()
switchWorkspace targetID seat stateMVar = do
  nextAction <-
    modifyMVar stateMVar $
      \state@WMState
         { allOutputWorkspaces
         , focusedOutput
         , allWindows
         , allWorkspacesFloating
         , lastFocusedWorkspace
         } -> do
          let
            currentFocusedWorkspace = allOutputWorkspaces B.! focusedOutput
          if
            | currentFocusedWorkspace == targetID && lastFocusedWorkspace == targetID -> pure (state, pure ())
            | currentFocusedWorkspace == targetID -> pure (state, switchWorkspace lastFocusedWorkspace seat stateMVar)
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
                    w S.:<| _ -> (Just w, riverSeatFocusWindow seat w)
                    S.Empty -> (Nothing, riverSeatClearFocus seat)

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

focusWindow :: WindowDirection -> Ptr RiverSeat -> MVar WMState -> IO ()
focusWindow direction seat stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Nothing -> pure state
      Just w -> do
        let focusedWorkspace = allOutputWorkspaces state B.! focusedOutput state
            tiledWindows = BS.lookupBs focusedWorkspace (allWorkspacesTiled state)
            floatingWindows = BS.lookupBs focusedWorkspace (allWorkspacesFloating state)
        case S.elemIndexL w tiledWindows of
          Just index -> do
            let geometries = ((fromMaybe (Rect 0 0 0 0)) . tilingGeometry . (allWindows state M.!)) <$> tiledWindows
                closestWindowIndex = findClosestWindow geometries direction index
                closestWindow = S.index tiledWindows closestWindowIndex
                Rect{rx, rw, ry, rh} = S.index geometries closestWindowIndex
            pure
              state
                { focusedWindow = Just closestWindow
                , manageQueue = manageQueue state >> riverSeatFocusWindow seat closestWindow >> riverSeatPointerWarp (focusedSeat state) (rx + rw `div` 2) (ry + rh `div` 2)
                }
          Nothing -> case S.elemIndexL w floatingWindows of
            Nothing -> pure state
            Just index -> do
              let windows = (allWindows state M.!) <$> floatingWindows
                  geometries = ((fromMaybe (Rect 0 0 0 0)) . floatingGeometry) <$> windows
                  closestWindowIndex = findClosestWindow geometries direction index
                  closestWindow = S.index floatingWindows closestWindowIndex
                  Rect{rx, rw, ry, rh} = S.index geometries closestWindowIndex
                  node = S.index (nodePtr <$> windows) closestWindowIndex
              pure
                state
                  { focusedWindow = Just closestWindow
                  , manageQueue =
                      manageQueue state
                        >> riverSeatFocusWindow seat closestWindow
                        >> riverSeatPointerWarp (focusedSeat state) (rx + rw `div` 2) (ry + rh `div` 2)
                  , renderQueue = renderQueue state >> riverNodePlaceTop node
                  }

swapWindow :: WindowDirection -> Ptr RiverSeat -> MVar WMState -> IO ()
swapWindow direction seat stateMVar =
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Nothing -> pure state
      Just w -> do
        let focusedWorkspace = allOutputWorkspaces state B.! focusedOutput state
            tiledWindows = BS.lookupBs focusedWorkspace (allWorkspacesTiled state)
        case S.elemIndexL w tiledWindows of
          Nothing -> pure state
          Just index -> do
            let geometries = ((fromMaybe (Rect 0 0 0 0)) . tilingGeometry . (allWindows state M.!)) <$> tiledWindows
                closestWindowIndex = findClosestWindow geometries direction index
                closestWindow = S.index tiledWindows closestWindowIndex
                swapWindows s = S.update closestWindowIndex w $ S.update index closestWindow s
                Rect{rx, rw, ry, rh} = S.index geometries closestWindowIndex
            pure
              state
                { allWorkspacesTiled = BS.changeSeqOrder focusedWorkspace swapWindows (allWorkspacesTiled state)
                , manageQueue = manageQueue state >> riverSeatPointerWarp seat (rx + rw `div` 2) (ry + rh `div` 2)
                }

findClosestWindow :: S.Seq Rect -> WindowDirection -> Int -> Int
findClosestWindow ws direction index = res
 where
  infinity = 1.0 / 0.0 :: Double
  Rect{rx, ry, rw, rh} = S.index ws index
  (res, _ :: Double) =
    S.foldlWithIndex
      ( \(oldI, oldDistance) newI newRect ->
          let distance = calculateDistance newRect
           in if distance < oldDistance then (newI, distance) else (oldI, oldDistance)
      )
      (index, infinity)
      ws
  calculateDistance :: Rect -> Double
  calculateDistance Rect{rx = x, ry = y, rw = w, rh = h} =
    if x == rx && y == ry
      then infinity
      else
        let dy = fromIntegral $ (ry + rh `div` 2) - (y + h `div` 2)
            dx = fromIntegral $ (rx + rw `div` 2) - (x + w `div` 2)
         in case direction of
              WindowLeft ->
                if dx < 0
                  then infinity
                  else (dx ** 2) + ((dy * 2) ** 2)
              WindowDown ->
                if dy > 0
                  then infinity
                  else (dy ** 2) + ((dx * 2) ** 2)
              WindowUp ->
                if dy < 0
                  then infinity
                  else (dy ** 2) + ((dx * 2) ** 2)
              WindowRight ->
                if dx > 0
                  then infinity
                  else (dx ** 2) + ((dy * 2) ** 2)

moveWindowToWorkspace :: WorkspaceID -> Ptr RiverSeat -> MVar WMState -> IO ()
moveWindowToWorkspace targetID seat stateMVar = do
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
       , manageQueue
       } -> do
        case focusedWindow state of
          Nothing -> pure state
          Just w -> do
            let Window{isFloating, isPinned, isFullscreen} = allWindows M.! w
            if allOutputWorkspaces B.! focusedOutput == targetID || isPinned
              then pure state
              else do
                let
                  newState
                    | isFullscreen = state{allWorkspacesFullscreen = BS.move w targetID allWorkspacesFullscreen}
                    | isFloating = state{allWorkspacesFloating = BS.move w targetID allWorkspacesFloating}
                    | otherwise = state{allWorkspacesTiled = BS.move w targetID allWorkspacesTiled}
                  remainingWindows = allWorkspaceWindows (allOutputWorkspaces B.! focusedOutput) newState
                  (nextFocus, focusAction) = case remainingWindows of
                    h S.:<| _ -> (Just h, riverSeatFocusWindow seat h)
                    S.Empty -> (Nothing, riverSeatClearFocus seat)
                riverWindowManagerManageDirty currentWmManager
                pure
                  newState
                    { renderQueue = renderQueue >> riverWindowHide w
                    , focusedWindow = nextFocus
                    , manageQueue = manageQueue >> focusAction
                    }

exec :: String -> Ptr RiverSeat -> MVar WMState -> IO ()
exec command _ _ = spawnCommand ("systemd-run --user --scope --slice=app.slice " ++ command) >> pure ()

reloadWindowManager :: FilePath -> Ptr RiverSeat -> MVar WMState -> IO ()
reloadWindowManager fp _ stateMVar = do
  modifyMVar_ stateMVar $
    \state@WMState
       { allWorkspacesTiled
       , allWorkspacesFloating
       , allWorkspacesFullscreen
       , allWindows
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
          newPersisted = PersistedState{persistedWindows = windowsToRecord}
        encodeFile fp newPersisted
        _ <- spawnCommand ("systemd-run --user --scope --slice=app.slice Rivermonad-reload")
        pure state

dragWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
dragWindow _ stateMVar = do
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

stopDragging :: Ptr RiverSeat -> MVar WMState -> IO ()
stopDragging seat stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    let stop = riverSeatOpEnd seat
    case opDeltaState state of
      Dragging ->
        case focusedWindow state of
          Nothing -> pure state
          Just w -> do
            let window = allWindows state M.! w
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
            ( (\Rect{rx, ry} -> calcDistance (rx, ry) (currentX, currentY))
                . (fromMaybe (Rect 0 0 0 0))
                . tilingGeometry
                . (allWindows state M.!)
            )
              <$> currentTiled
          index =
            case distances of
              S.Empty -> 0
              h S.:<| t -> fst $ S.foldlWithIndex (\(oldI, old) i newD -> if newD < old then (i + 1, newD) else (oldI, old)) (0, h) t
          newTiled = BS.insertByIndex focusedWorkspace w (fromIntegral index) (allWorkspacesTiled state)
        pure
          state
            { manageQueue = manageQueue state >> stop
            , opDeltaState = None
            , allWorkspacesTiled = newTiled
            , currentOpDelta = (0, 0, 0, 0)
            }
      _ -> pure state{manageQueue = manageQueue state >> stop}

resizeWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
resizeWindow seat stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Nothing -> pure state
      Just w -> do
        let
          win@Window{isFullscreen, isFloating} = (allWindows state M.! w)
        if
          | isFullscreen -> pure state
          | isFloating ->
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

                  riverSeatOpStartPointer seat
                  pure
                    state
                      { opDeltaState = Resizing edge
                      , manageQueue = manageQueue state >> riverWindowInformResizeStart w
                      }
          | otherwise -> do
              riverSeatOpStartPointer seat
              pure
                state
                  { opDeltaState = ResizingTile
                  , manageQueue = manageQueue state >> riverWindowInformResizeStart w
                  }

stopResizing :: Ptr RiverSeat -> MVar WMState -> IO ()
stopResizing seat stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    case focusedWindow state of
      Nothing -> pure state
      Just win -> do
        let
          window = allWindows state M.! win
          stop = riverSeatOpEnd seat
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

startRepeating :: (Ptr RiverSeat -> MVar WMState -> IO ()) -> Ptr RiverSeat -> MVar WMState -> IO ()
startRepeating action seat stateMVar =
  modifyMVar_ stateMVar $ \state -> do
    -- Ensure we don't start two repeaters for the same key
    case activeRepeater state of
      Just _ -> return state
      Nothing -> do
        print "Repeat"
        hFlush stdout
        tid <- forkIO $ do
          print "hello"
          action seat stateMVar -- Initial press
          print "hello1"
          threadDelay 500000 -- Initial delay (0.5s)
          print "hello2"
          hFlush stdout
          forever $ do
            action seat stateMVar
            threadDelay 50000 -- Repeat rate (20Hz)
            print "hello3"
            riverWindowManagerManageDirty $ currentWmManager state
        return state{activeRepeater = Just tid}

stopRepeating :: Ptr RiverSeat -> MVar WMState -> IO ()
stopRepeating _ stateMVar = modifyMVar_ stateMVar $ \state -> do
  case activeRepeater state of
    Nothing -> return state
    Just tid -> do
      killThread tid
      return state{activeRepeater = Nothing}
