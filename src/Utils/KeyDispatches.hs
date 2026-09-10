{-# LANGUAGE MultiWayIf #-}

module Utils.KeyDispatches (
  closeCurrentWindow,
  closeAllWindowsOnWorkspace,
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
  setOutputPresentationMode,
) where

import Control.Concurrent
import Control.Monad (forM_, unless, void, when)
import Control.Monad.State hiding (state)
import Data.Aeson (encodeFile)
import Data.Bimap qualified as B
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as S
import IPC
import Optics.Core
import Optics.State
import Optics.State.Operators
import Protocols.Generated
import System.Process
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.Connection

doNothing :: Object RiverSeatV1 -> MVar WMState -> W ()
doNothing _ _ = pure ()

sendMessage :: (Message m) => m -> Object RiverSeatV1 -> MVar WMState -> W ()
sendMessage msg _ stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform = do
    use focusedWorkspace >>= \case
      Nothing -> pure ()
      Just ws -> do
        layouts <- use #workspaceLayouts
        forM_ (handleSomeMsg (layouts M.! ws) (SomeMessage msg)) $ \l -> #workspaceLayouts % at ws ?= l

exitSession :: Object RiverSeatV1 -> MVar WMState -> W ()
exitSession _ stateMVar = (liftIO $ readMVar stateMVar) >>= riverWindowManagerV1ExitSession . currentWM

closeCurrentWindow :: Object RiverSeatV1 -> MVar WMState -> W ()
closeCurrentWindow _ stateMVar = do
  modifyMVarW_ stateMVar $ \state ->
    case state ^. #focusedWin of
      Nothing -> pure state
      Just w -> pure $ state & #manageQueue >>~ riverWindowV1Close w

closeAllWindowsOnWorkspace :: Object RiverSeatV1 -> MVar WMState -> W ()
closeAllWindowsOnWorkspace _ stateMVar = do
  modifyMVarW_ stateMVar $ \state ->
    case state ^. focusedWorkspace of
      Nothing -> pure state
      Just ws -> do
        let wins = state ^. #allWorkspacesTiled % to (BS.lookupBs ws)
            wins2 = state ^. #allWorkspacesFloating % to (BS.lookupBs ws)
            wins3 = state ^. #allWorkspacesFullscreen % to (BS.lookupBs ws)
            actions = foldl' (\b a -> b >> riverWindowV1Close a) (pure ())
        pure $ state & #manageQueue >>~ (actions wins >> actions wins2 >> actions wins3)

toggleFocusFloating :: Object RiverSeatV1 -> MVar WMState -> W ()
toggleFocusFloating _ stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform =
    use #focusedWin >>= \case
      Nothing -> pure ()
      Just w ->
        use (pairOfGetter (#allWindows % at w) focusedWorkspace) >>= \case
          (Just win, Just ws) | not (win ^. #winFull) -> do
            let targetOptic
                  | view #winFloat win = #allWorkspacesTiled
                  | otherwise = #allWorkspacesFloating
            preuse (targetOptic % to (BS.lookupBs ws) % _head) >>= \case
              Just next -> setFocusedWindowAndHistory ws next
              Nothing -> pure ()
          _ -> pure ()

cycleWindowFocus :: Bool -> Object RiverSeatV1 -> MVar WMState -> W ()
cycleWindowFocus forward _ stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform =
    use (pairOfGetter #focusedWin focusedWorkspace) >>= \case
      (Just w, Just focusedWs) ->
        use (#allWindows % at w) >>= \case
          Just win -> do
            let targetMapOptic
                  | view #winFull win = #allWorkspacesFullscreen
                  | view #winFloat win = #allWorkspacesFloating
                  | otherwise = #allWorkspacesTiled

            next <- BS.lookUpNext focusedWs forward w <$> use targetMapOptic

            nextWinData <- use (#allWindows % at next)
            let renderAction = case nextWinData of
                  Just nData | view #winFull win || view #winFloat win -> riverNodeV1PlaceTop (view #winNodeObj nData)
                  _ -> pure ()

            setFocusedWindowAndHistory focusedWs next
            #renderQueue >>>= renderAction
          _ -> pure ()
      _ -> pure ()

toggleFullscreenCurrentWindow :: Object RiverSeatV1 -> MVar WMState -> W ()
toggleFullscreenCurrentWindow _ stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform = do
    use (pairOfGetter #focusedWin focusedWorkspace) >>= \case
      (Just win, Just ws) -> do
        use (#allWindows % at win) >>= \case
          Just winRec | not (winRec ^. #winPinned) -> do
            let currentlyFullscreen = winRec ^. #winFull
                currentlyFloating = winRec ^. #winFloat
            if currentlyFullscreen
              then exitFullscreen win currentlyFloating ws
              else enterFullscreen win currentlyFloating ws
            #allWindows % at win %? #winFull %= not
          _ -> pure ()
      _ -> pure ()

  enterFullscreen win isFloating ws = do
    if isFloating
      then #allWorkspacesFloating %= BS.delete win
      else #allWorkspacesTiled %= BS.delete win
    #fullscreenQueue % at ws %?= (win :)

  exitFullscreen win isFloating ws = do
    #allWorkspacesFullscreen %= BS.delete win
    if isFloating
      then #floatingQueue % at ws %?= (win :)
      else #allWorkspacesTiled %= BS.insert ws win
    #manageQueue >>>= (riverWindowV1ExitFullscreen win >> riverWindowV1InformNotFullscreen win)

toggleFloatingCurrentWindow :: Object RiverSeatV1 -> MVar WMState -> W ()
toggleFloatingCurrentWindow _ stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform =
    use (pairOfGetter #focusedWin focusedWorkspace) >>= \case
      (Just win, Just ws) -> do
        use (#allWindows % at win) >>= \case
          Just winRec | not (winRec ^. #winPinned || winRec ^. #winFull) -> do
            if view #winFloat winRec
              then exitFloating win ws
              else enterFloating win ws
            #allWindows % at win %? #winFloat %= not
          _ -> pure ()
      _ -> pure ()

  enterFloating win ws = do
    #allWorkspacesTiled %= BS.delete win
    #floatingQueue % at ws %?= (win :)

  exitFloating win ws = do
    #allWorkspacesFloating %= BS.delete win
    #allWorkspacesTiled %= BS.insert ws win

togglePinWindow :: Object RiverSeatV1 -> MVar WMState -> W ()
togglePinWindow _ stateMVar = do
  modifyMVarW_ stateMVar $ \s ->
    case s ^. #focusedWin of
      Nothing -> pure s
      Just w -> case s ^? #allWindows % at w % _Just of
        Just win | win ^. #winFloat && not (win ^. #winFull) -> pure $ s & #allWindows % at w %? #winPinned %~ not
        _ -> pure s

toggleMaximizeWindow :: Object RiverSeatV1 -> MVar WMState -> W ()
toggleMaximizeWindow _ stateMVar = do
  modifyMVarW_ stateMVar $ \s ->
    case s ^. #focusedWin of
      Nothing -> pure s
      Just w -> case s ^? #allWindows % at w % _Just of
        Nothing -> pure s
        Just Window{winMaximized} ->
          pure $
            s
              & (#allWindows % at w %? #winMaximized %~ not)
              & (#manageQueue >>~ if winMaximized then riverWindowV1InformUnmaximized w else riverWindowV1InformMaximized w)

cycleWindows :: Bool -> Object RiverSeatV1 -> MVar WMState -> W ()
cycleWindows forward _ stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform =
    use (pairOfGetter #focusedWin focusedWorkspace) >>= \case
      (Just w, Just focusedWs) -> do
        #allWorkspacesTiled %= BS.changeSeqOrder focusedWs (cycleW forward)
        tiledMap <- use #allWorkspacesTiled
        case BS.lookupA w tiledMap of
          Nothing -> pure ()
          Just workspace -> do
            let nextWin = BS.lookUpNext workspace forward w tiledMap
            setFocusedWindowAndHistory focusedWs nextWin
      _ -> pure ()

  cycleW _ S.Empty = S.empty
  cycleW True (h S.:<| hs) = hs S.|> h
  cycleW False (hs S.:|> h) = h S.<| hs

cycleWindowSlaves :: Bool -> Object RiverSeatV1 -> MVar WMState -> W ()
cycleWindowSlaves forward _ stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform = do
    use (pairOfGetter #focusedWin focusedWorkspace) >>= \case
      (Just w, Just focusedWs) -> do
        #allWorkspacesTiled %= BS.changeSeqOrder focusedWs (cycleW forward)
        tiledMap <- use #allWorkspacesTiled
        let s = BS.lookupBs focusedWs tiledMap
        case S.elemIndexL w s of
          Just i | i /= 0 -> do
            let nextWin = S.index s (((if forward then i else i - 2) `mod` (length s - 1)) + 1)
            setFocusedWindowAndHistory focusedWs nextWin
          _ -> pure ()
      _ -> pure ()

  cycleW True (h S.:<| (slaveH S.:<| hs)) = h S.<| (hs S.|> slaveH)
  cycleW False (h S.:<| (hs S.:|> slaveH)) = h S.<| (slaveH S.<| hs)
  cycleW _ hs = hs

zoomWindow :: Object RiverSeatV1 -> MVar WMState -> W ()
zoomWindow _ stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform = do
    use (pairOfGetter #focusedWin focusedWorkspace) >>= \case
      (Just currentWin, Just ws) -> do
        tiledWindows <- use (#allWorkspacesTiled % to (BS.lookupBs ws))
        let (newSeq, newFocus) = zoom currentWin tiledWindows
        #allWorkspacesTiled %= BS.changeSeqOrder ws (const newSeq)
        #focusedWin ?= newFocus
      _ -> pure ()

  zoom c S.Empty = (S.empty, c)
  zoom currentWin s@(w S.:<| ws)
    | w == currentWin = case ws of
        S.Empty -> (s, currentWin)
        w2 S.:<| wss -> (w2 S.<| (w S.<| wss), w2)
    | otherwise = case S.elemIndexL currentWin ws of
        Nothing -> (s, currentWin)
        Just i -> (currentWin S.<| S.update i w ws, currentWin)

-- Does not move floating windows on another monitor
switchWorkspace :: WorkspaceID -> Object RiverSeatV1 -> MVar WMState -> W ()
switchWorkspace targetID _ stateMVar = modifyMVarW_ stateMVar $ \state -> do
  let newState = execState (transform targetID) state
  liftIO $ broadcastState newState $ formatStatus newState
 where
  transform target = do
    currentO <- use #focusedOut
    outWorkmaps <- use #allOutputWorkspaces
    lastWs <- use #lastFocusedWorkspace
    case B.lookup currentO outWorkmaps of
      Just currentWs | currentWs /= target -> do
        #allOutputWorkspaces %= B.insert currentO target
        -- Pinned windows are moved to new workspace
        use #allWindows >>= itraverseOf_ (itraversed % filtered (^. #winPinned)) (\p _ -> #allWorkspacesFloating %= BS.move p target)

        case B.lookupR target outWorkmaps of
          Nothing -> do
            -- Hide old windows, show new windows (including pinned)
            newWins <- use (workspaceWindows target)
            currentWins <- use (workspaceWindows currentWs)
            #renderQueue >>>= (mapM_ riverWindowV1Show newWins >> mapM_ riverWindowV1Hide currentWins)
          Just o2 -> do
            #allOutputWorkspaces %= B.insert o2 currentWs
            -- Refullscreen old fullscreen windows on new monitor (old workspace)
            fullscreened <- use #allWorkspacesFullscreen
            forM_ (BS.lookupBs target fullscreened) $ \w ->
              do
                #allWorkspacesFullscreen %= BS.delete w
                #fullscreenQueue % at target %?= (w :)
            forM_ (BS.lookupBs currentWs fullscreened) $ \w ->
              do
                #allWorkspacesFullscreen %= BS.delete w
                #fullscreenQueue % at currentWs %?= (w :)

        #lastFocusedWorkspace .= currentWs
        use (#workspaceFocusHistory % at target) >>= \case
          Nothing ->
            use (workspaceWindows target) >>= \case
              w S.:<| _ -> setFocusedWindowAndHistory target w
              S.Empty -> #focusedWin .= Nothing
          Just w -> #focusedWin ?= w
      Just _ | lastWs /= target -> transform lastWs
      _ -> pure ()

formatStatus :: WMState -> String
formatStatus state =
  let
    windows = (\i -> (i, (state ^. workspaceWindows i))) <$> [1 .. 9]
    target = fromMaybe 1 $ B.lookup (focusedOut state) (allOutputWorkspaces state)
    str = concat $ L.intersperse "," $ fmap (\(i, s) -> if i == target then "1" else if S.length s > 0 then "2" else "0") windows
   in
    "tags:" ++ str

focusWindow :: WindowDirection -> Object RiverSeatV1 -> MVar WMState -> W ()
focusWindow direction seat stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform =
    use (pairOfGetter #focusedWin focusedWorkspace) >>= \case
      (Just currentWin, Just ws) -> do
        tiled <- use (#allWorkspacesTiled % to (BS.lookupBs ws))
        case S.elemIndexL currentWin tiled of
          Just idx -> do
            geoms <- getGeometries tiled #winTileGeo
            shiftFocus idx tiled geoms False ws
          Nothing -> do
            floating <- use (#allWorkspacesFloating % to (BS.lookupBs ws))
            case S.elemIndexL currentWin floating of
              Just idx -> do
                geoms <- getGeometries floating #winFloatGeo
                shiftFocus idx floating geoms True ws
              Nothing -> pure ()
      _ -> pure ()

  getGeometries ptrs geoField = do
    allWins <- use #allWindows
    pure $ ptrs <&> \ptr -> fromMaybe (Rect 0 0 0 0) (allWins ^? at ptr %? geoField % _Just)

  shiftFocus idx ptrs geoms isFloating ws = do
    let nextIdx = findClosestWindow geoms direction idx
        nextWin = S.index ptrs nextIdx
        rect = S.index geoms nextIdx
        centerX = rx rect + rw rect `div` 2
        centerY = ry rect + rh rect `div` 2

    setFocusedWindowAndHistory ws nextWin

    #manageQueue >>>= riverSeatV1PointerWarp seat centerX centerY

    when isFloating $ do
      mNode <- preuse (#allWindows % at nextWin %? #winNodeObj)
      forM_ mNode $ \node -> #renderQueue >>>= riverNodeV1PlaceTop node

swapWindow :: WindowDirection -> Object RiverSeatV1 -> MVar WMState -> W ()
swapWindow direction seat stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  getGeometries ptrs geoField = do
    allWins <- use #allWindows
    pure $ ptrs <&> \ptr -> fromMaybe (Rect 0 0 0 0) (allWins ^? at ptr %? geoField % _Just)
  transform = do
    use (pairOfGetter #focusedWin focusedWorkspace) >>= \case
      (Just currentWin, Just ws) -> do
        tiled <- use (#allWorkspacesTiled % to (BS.lookupBs ws))
        case S.elemIndexL currentWin tiled of
          Nothing -> pure ()
          Just idx -> do
            geoms <- getGeometries tiled #winTileGeo
            let nextIdx = findClosestWindow geoms direction idx
                nextWin = S.index tiled nextIdx
                rect = S.index geoms nextIdx
                centerX = rx rect + rw rect `div` 2
                centerY = ry rect + rh rect `div` 2

            #allWorkspacesTiled %= BS.changeSeqOrder ws (S.update nextIdx currentWin . S.update idx nextWin)
            #manageQueue >>>= riverSeatV1PointerWarp seat centerX centerY
      _ -> pure ()

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
                if dx <= 0
                  then infinity
                  else (dx ** 2) + ((dy * 4) ** 2)
              WindowDown ->
                if dy >= 0
                  then infinity
                  else (dy ** 2) + ((dx * 4) ** 2)
              WindowUp ->
                if dy <= 0
                  then infinity
                  else (dy ** 2) + ((dx * 4) ** 2)
              WindowRight ->
                if dx >= 0
                  then infinity
                  else (dx ** 2) + ((dy * 4) ** 2)

moveWindowToWorkspace :: WorkspaceID -> Object RiverSeatV1 -> MVar WMState -> W ()
moveWindowToWorkspace targetID _ stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform = do
    use (pairOfGetter #focusedWin focusedWorkspace) >>= \case
      (Just win, Just currentWS)
        | currentWS /= targetID ->
            use (#allWindows % at win) >>= \case
              Just winRec | not (view #winPinned winRec) -> do
                moveWindowStructural win winRec
                #workspaceFocusHistory % at targetID ?= win
                #renderQueue >>>= riverWindowV1Hide win

                use (workspaceWindows currentWS) >>= \case
                  (h S.:<| _) -> setFocusedWindowAndHistory currentWS h
                  S.Empty -> do
                    #focusedWin .= Nothing
                    #workspaceFocusHistory % at currentWS .= Nothing
              _ -> pure ()
      _ -> pure ()

  moveWindowStructural win winRec
    | view #winFull winRec = #allWorkspacesFullscreen %= BS.move win targetID
    | view #winFloat winRec = #allWorkspacesFloating %= BS.move win targetID
    | otherwise = #allWorkspacesTiled %= BS.move win targetID

exec :: String -> Object RiverSeatV1 -> MVar WMState -> W ()
exec command _ _ =
  void $
    liftIO $
      createProcess $
        (shell ("systemd-run --user --scope --slice=app.slice " ++ command)){close_fds = True}

reloadWindowManager :: FilePath -> Object RiverSeatV1 -> MVar WMState -> W ()
reloadWindowManager fp _ stateMVar = do
  state <- liftIO $ readMVar stateMVar

  let windowsToRecord = M.fromList $ toPersistedEntry <$> (M.elems $ state ^. #allWindows)
      workspacesToRecord = M.fromList $ (\(o, w) -> (view #outWlOut $ (state ^. #allOutputs) M.! o, w)) <$> B.toList (state ^. #allOutputWorkspaces)
      newPersisted = PersistedState{persistedWindows = windowsToRecord, persistedOutputs = workspacesToRecord}
      toPersistedEntry w = (ident, (fromMaybe 1 $ BS.lookupA obj ws, status))
       where
        ident = w ^. #winIdentifier
        obj = w ^. #winObj
        ws
          | w ^. #winFloat && w ^. #winFull = state ^. #allWorkspacesFullscreen
          | w ^. #winFull = state ^. #allWorkspacesFullscreen
          | w ^. #winFloat = state ^. #allWorkspacesFloating
          | otherwise = state ^. #allWorkspacesTiled
        status
          | w ^. #winFloat && w ^. #winFull = FullscreenFloating
          | w ^. #winFull = Fullscreen
          | w ^. #winFloat = Floating
          | otherwise = Tiled
  liftIO $ encodeFile fp newPersisted
  void $ liftIO $ spawnCommand "systemd-run --user --scope --slice=app.slice Rivermonad-reload"

dragWindow :: Object RiverSeatV1 -> MVar WMState -> W ()
dragWindow seat stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform = do
    mWin <- use #focusedWin
    forM_ mWin $ \win -> do
      mWinRec <- use (#allWindows % at win)
      forM_ mWinRec $ \winRec -> unless (winRec ^. #winFull) $ do
        setCursorShape seat WpCursorShapeDeviceV1ShapeGrabbing
        #manageQueue >>>= riverSeatV1OpStartPointer seat
        if winRec ^. #winFloat
          then #opDeltaState .= Dragging
          else do
            let Rect{rx, ry} = winRec ^. #winTileGeo % non (Rect 0 0 0 0)
            #opDeltaState .= DraggingTile
            #currentOpDelta .= (rx, ry, 0, 0)
            #allWorkspacesTiled %= BS.delete win

stopDragging :: Object RiverSeatV1 -> MVar WMState -> W ()
stopDragging seat stateMVar = modifyMVarW_ stateMVar $ pure . execState finalizeDrag
 where
  finalizeDrag = do
    use (pairOfGetter #focusedWin #opDeltaState) >>= \case
      (Just win, Dragging) -> do
        (newX, newY, _, _) <- use #currentOpDelta
        #allWindows % at win %? #winFloatGeo %?= \r -> r{rx = newX, ry = newY}
      (Just win, DraggingTile) -> do
        ws <- use (focusedWorkspace % non 1)

        (curX, curY, _, _) <- use #currentOpDelta
        tiledList <- use (#allWorkspacesTiled % to (BS.lookupBs ws))
        allWins <- use #allWindows

        let getCoord p = allWins ^? at p %? #winTileGeo % _Just
            dist r = sqrt $ fromIntegral ((r ^. #rx - curX) ^ (2 :: Int) + (r ^. #ry - curY) ^ (2 :: Int))
            distances :: S.Seq Double
            distances = fmap (dist . fromMaybe (Rect 0 0 0 0) . getCoord) tiledList

            targetIndex = case distances of
              S.Empty -> 0
              h S.:<| t -> fst $ S.foldlWithIndex (\(oldI, oldD) i newD -> if newD < oldD then (i + 1, newD) else (oldI, oldD)) (0, h) t

        #allWorkspacesTiled %= BS.insertByIndex ws win (fromIntegral targetIndex)
      _ -> pure ()
    #opDeltaState .= None
    #currentOpDelta .= (0, 0, 0, 0)
    #manageQueue >>>= riverSeatV1OpEnd seat
    setCursorShape seat WpCursorShapeDeviceV1ShapeDefault

resizeWindow :: Object RiverSeatV1 -> MVar WMState -> W ()
resizeWindow seat stateMVar = modifyMVarW_ stateMVar $ pure . execState startResize
 where
  startResize = do
    mWin <- use #focusedWin
    forM_ mWin $ \win -> do
      mWinRec <- use (#allWindows % at win)
      forM_ mWinRec $ \winRec -> do
        #manageQueue >>>= riverSeatV1OpStartPointer seat
        #manageQueue >>>= riverWindowV1InformResizeStart win
        if
          | winRec ^. #winFloat -> forM_ (winRec ^. #winFloatGeo) $ \Rect{rx, ry, rw, rh} -> do
              (cX, cY) <- use #cursorPosition
              let (edge, shape)
                    | cX < firstX && cY < firstY = (edgeTopLeft, WpCursorShapeDeviceV1ShapeNwResize)
                    | cX < secondX && cY < firstY = (edgeTop, WpCursorShapeDeviceV1ShapeNResize)
                    | cY < firstY = (edgeTopRight, WpCursorShapeDeviceV1ShapeNeResize)
                    | cX < firstX && cY < secondY = (edgeLeft, WpCursorShapeDeviceV1ShapeWResize)
                    | cX < oneHalfX && cY < oneHalfY = (edgeTopLeft, WpCursorShapeDeviceV1ShapeNwResize)
                    | cX < secondX && cY < oneHalfY = (edgeTopRight, WpCursorShapeDeviceV1ShapeNeResize)
                    | cX < oneHalfX && cY < secondY = (edgeBottomLeft, WpCursorShapeDeviceV1ShapeSwResize)
                    | cX < secondX && cY < secondY = (edgeBottomRight, WpCursorShapeDeviceV1ShapeSeResize)
                    | cY < secondY = (edgeRight, WpCursorShapeDeviceV1ShapeEResize)
                    | cX < firstX = (edgeBottomLeft, WpCursorShapeDeviceV1ShapeSwResize)
                    | cX < secondX = (edgeBottom, WpCursorShapeDeviceV1ShapeSResize)
                    | otherwise = (edgeBottomRight, WpCursorShapeDeviceV1ShapeSeResize)
                   where
                    oneThirdW = rw `div` 3
                    oneThirdH = rh `div` 3
                    oneHalfX = rx + rw `div` 2
                    oneHalfY = ry + rh `div` 2
                    firstX = rx + oneThirdW
                    secondX = firstX + oneThirdW
                    firstY = ry + oneThirdH
                    secondY = firstY + oneThirdH
              setCursorShape seat shape
              #opDeltaState .= Resizing edge
          | winRec ^. #winFull -> pure ()
          | otherwise -> #opDeltaState .= ResizingTile

stopResizing :: Object RiverSeatV1 -> MVar WMState -> W ()
stopResizing seat stateMVar = modifyMVarW_ stateMVar $ pure . execState finalizeResize
 where
  finalizeResize = do
    mWin <- use #focusedWin
    forM_ mWin $ \win -> do
      use #opDeltaState >>= \case
        Resizing _ -> do
          (x, y, w, h) <- use #currentOpDelta
          #allWindows % at win %? #winFloatGeo %?= \r -> r{rx = x, ry = y, rw = w, rh = h}
        _ -> pure ()

      #manageQueue >>>= riverSeatV1OpEnd seat
      setCursorShape seat WpCursorShapeDeviceV1ShapeDefault
      #manageQueue >>>= riverWindowV1InformResizeEnd win
      #opDeltaState .= None
      #currentOpDelta .= (0, 0, 0, 0)

setCursorShape :: Object RiverSeatV1 -> WpCursorShapeDeviceV1ShapeEnum -> State WMState ()
setCursorShape seat shape = do
  preuse (#allSeats % at seat %? #seatWlSeat) >>= \case
    Nothing -> pure ()
    Just name ->
      preuse (#allWlSeats % at name %? #wlCursorShapeDevice % _Just) >>= \case
        Just device ->
          preuse (#allWlSeats % at name %? #wlPointerSerial) >>= \case
            Nothing -> pure ()
            Just serial -> #manageQueue >>>= wpCursorShapeDeviceV1SetShape device serial shape
        _ -> pure ()

setOutputPresentationMode :: RiverOutputV1PresentationModeEnum -> Object RiverSeatV1 -> MVar WMState -> W ()
setOutputPresentationMode mode _ stateMVar = modifyMVarW_ stateMVar $ pure . execState transform
 where
  transform = do
    o <- use #focusedOut
    unless (o == nonObject) $ #renderQueue >>>= riverOutputV1SetPresentationMode o mode
