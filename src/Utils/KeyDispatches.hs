{-# LANGUAGE MultiWayIf #-}

module Utils.KeyDispatches (
  closeCurrentWindow,
  cycleWindowFocus,
  cycleWindowSlaves,
  cycleWindows,
  doNothing,
  dragWindow,
  exec,
  exitSession,
  focusWindow,
  moveWindowToWorkspace,
  reloadWindowManager,
  resizeWindow,
  sendMessage,
  stopDragging,
  stopResizing,
  swapWindow,
  switchWorkspace,
  toggleFloatingCurrentWindow,
  toggleFocusFloating,
  toggleFullscreenCurrentWindow,
  toggleMaximizeWindow,
  togglePinWindow,
  zoomWindow,
) where

import Control.Concurrent
import Control.Monad (unless)
import Control.Monad.State hiding (state)
import Data.Aeson (encodeFile)
import Data.Bimap qualified as B
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as S
import Foreign
import IPC
import Optics.Core
import Optics.State
import Optics.State.Operators
import System.Process
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.ImportedFunctions

doNothing :: Ptr RiverSeat -> MVar WMState -> IO ()
doNothing _ _ = pure ()

sendMessage :: (Message m) => m -> Ptr RiverSeat -> MVar WMState -> IO ()
sendMessage msg _ stateMVar = do
  modifyMVar_ stateMVar $ \state -> do
    let focusedWorkspace = allOutputWorkspaces state B.! focusedOutput state
    case handleSomeMsg (workspaceLayouts state M.! focusedWorkspace) (SomeMessage msg) of
      Nothing -> pure state
      Just layout -> do
        pure state{workspaceLayouts = M.insert focusedWorkspace layout (workspaceLayouts state)}

exitSession :: Ptr RiverSeat -> MVar WMState -> IO ()
exitSession _ stateMVar =
  modifyMVar_ stateMVar $ \state -> do
    riverWindowManagerExitSession (currentWmManager state)
    pure state

closeCurrentWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
closeCurrentWindow _ stateMVar = do
  modifyMVar_ stateMVar $ \state ->
    case state ^. #focusedWindow of
      Nothing -> pure state
      Just w ->
        pure $ state & #manageQueue %~ (>> riverWindowClose w)

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
cycleWindowFocus forward seat stateMVar = modifyMVar_ stateMVar $ \state -> do
  let newState = execState transform state
  pure newState
 where
  transform = do
    maybeFocused <- use #focusedWindow
    case maybeFocused of
      Nothing -> pure ()
      Just w -> do
        maybeWinData <- preuse (#allWindows % at w % _Just)
        fOutput <- use #focusedOutput
        outWorkmaps <- use #allOutputWorkspaces

        case (maybeWinData, B.lookup fOutput outWorkmaps) of
          (Just win, Just focusedWorkspace) -> do
            let targetMapOptic
                  | view #isFullscreen win = #allWorkspacesFullscreen
                  | view #isFloating win = #allWorkspacesFloating
                  | otherwise = #allWorkspacesTiled

            targetMap <- use targetMapOptic

            let next = BS.lookUpNext focusedWorkspace forward w targetMap

            nextWinData <- preuse (#allWindows % at next % _Just)
            let renderAction = case nextWinData of
                  Just nData
                    | view #isFullscreen win || view #isFloating win ->
                        riverNodePlaceTop (view #nodePtr nData)
                  _ -> pure ()

            #focusedWindow .= Just next
            #renderQueue %= (>> renderAction)
            #manageQueue %= (>> riverSeatFocusWindow seat next)
          _ -> pure ()

toggleFullscreenCurrentWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
toggleFullscreenCurrentWindow _ stateMVar = modifyMVar_ stateMVar $ \state ->
  pure $ execState transform state
 where
  transform = do
    mWinPtr <- use #focusedWindow
    fOutput <- use #focusedOutput
    workmaps <- use #allOutputWorkspaces
    case (mWinPtr, B.lookup fOutput workmaps) of
      (Just win, Just ws) -> do
        mWin <- preuse (#allWindows % at win % _Just)
        case mWin of
          Just winRec | not (winRec ^. #isPinned) -> do
            let currentlyFullscreen = view #isFullscreen winRec
                currentlyFloating = view #isFloating winRec
            if currentlyFullscreen
              then exitFullscreen win currentlyFloating ws
              else enterFullscreen win currentlyFloating ws
            #allWindows % at win % _Just % #isFullscreen %= not
          _ -> pure ()
      _ -> pure ()

  enterFullscreen win isFloating ws = do
    if isFloating
      then #allWorkspacesFloating %= BS.delete win
      else #allWorkspacesTiled %= BS.delete win
    #fullscreenQueue % at ws % _Just %= (win :)

  exitFullscreen win isFloating ws = do
    #allWorkspacesFullscreen %= BS.delete win
    if isFloating
      then #floatingQueue % at ws % _Just %= (win :)
      else #allWorkspacesTiled %= BS.insert ws win
    #manageQueue %= (>> (riverWindowExitFullscreen win >> riverWindowInformNotFullscreen win))

toggleFloatingCurrentWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
toggleFloatingCurrentWindow _ stateMVar = modifyMVar_ stateMVar $ \state ->
  pure $ execState transform state
 where
  transform = do
    mWinPtr <- use #focusedWindow
    fOutput <- use #focusedOutput
    workmaps <- use #allOutputWorkspaces
    case (mWinPtr, B.lookup fOutput workmaps) of
      (Just win, Just ws) -> do
        mWin <- preuse (#allWindows % at win % _Just)
        case mWin of
          Just winRec | not (winRec ^. #isPinned || winRec ^. #isFullscreen) -> do
            if view #isFloating winRec
              then exitFloating win ws
              else enterFloating win ws
            #allWindows % at win % _Just % #isFloating %= not
          _ -> pure ()
      _ -> pure ()

  enterFloating win ws = do
    #allWorkspacesTiled %= BS.delete win
    #floatingQueue % at ws % _Just %= (win :)

  exitFloating win ws = do
    #allWorkspacesFloating %= BS.delete win
    #allWorkspacesTiled %= BS.insert ws win

togglePinWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
togglePinWindow _ stateMVar = do
  modifyMVar_ stateMVar $ \s ->
    case s ^. #focusedWindow of
      Nothing -> pure s
      Just w -> case s ^? #allWindows % at w % _Just of
        Just win | win ^. #isFloating && not (win ^. #isFullscreen) -> pure $ s & #allWindows % at w % _Just % #isPinned %~ not
        _ -> pure s

toggleMaximizeWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
toggleMaximizeWindow _ stateMVar = do
  modifyMVar_ stateMVar $ \s ->
    case s ^. #focusedWindow of
      Nothing -> pure s
      Just w -> case s ^? #allWindows % at w % _Just of
        Nothing -> pure s
        Just win@Window{isMaximized} ->
          pure $
            s
              & (#allWindows % at w ?~ (win & #isMaximized %~ not))
              & (#manageQueue %~ (>> if isMaximized then riverWindowInformUnmaximized w else riverWindowInformMaximized w))

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
zoomWindow _ stateMVar = modifyMVar_ stateMVar $ \state ->
  pure $ execState transform state
 where
  transform = do
    mFocusedWin <- use #focusedWindow
    fOutput <- use #focusedOutput
    outWorkmaps <- use #allOutputWorkspaces
    case (mFocusedWin, B.lookup fOutput outWorkmaps) of
      (Just currentWin, Just workspace) -> do
        mWin <- preuse (#allWindows % at currentWin % _Just)
        let shouldSkip = case mWin of
              Just w -> view #isFloating w || view #isFullscreen w
              Nothing -> True
        unless shouldSkip $ #allWorkspacesTiled %= BS.changeSeqOrder workspace (zoom currentWin)
      _ -> pure ()
  zoom currentWin s = case s of
    S.Empty -> s
    w S.:<| ws
      | w == currentWin -> case ws of
          S.Empty -> s
          w2 S.:<| wss -> w2 S.<| (w S.<| wss)
      | otherwise -> case S.elemIndexL currentWin ws of
          Nothing -> s
          Just i -> currentWin S.<| S.update i w ws

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

                newState <- broadcastState state (formatStatus state targetID)

                pure
                  ( newState
                      { renderQueue = renderQueue state >> hidingActions >> showingActions
                      , manageQueue = manageQueue state >> focusAction
                      , allOutputWorkspaces = newOutputWorkspaces
                      , lastFocusedWorkspace = currentFocusedWorkspace
                      , allWorkspacesFloating = newWorkspacesFloating
                      , focusedWindow = newFocusedWindow
                      }
                  , pure ()
                  )
  nextAction

formatStatus :: WMState -> WorkspaceID -> String
formatStatus state target =
  let
    windows = fmap (\i -> (i, allWorkspaceWindows i state)) [1 .. 9]
    str = concat $ L.intersperse "," $ fmap (\(i, s) -> if i == target then "1" else if S.length s > 0 then "2" else "0") windows
   in
    "tags:" ++ str

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
