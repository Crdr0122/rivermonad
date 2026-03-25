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
import Control.Monad (forM_, unless, void, when)
import Control.Monad.State hiding (state)
import Data.Aeson (encodeFile)
import Data.Bimap qualified as B
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as S
import Foreign hiding (void)
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
sendMessage msg _ stateMVar = modifyMVar_ stateMVar $ pure . execState transform
 where
  transform = do
    use focusedWorkspace >>= \case
      Nothing -> pure ()
      Just ws -> do
        layouts <- use #workspaceLayouts
        forM_ (handleSomeMsg (layouts M.! ws) (SomeMessage msg)) $ \l -> #workspaceLayouts % at ws ?= l

exitSession :: Ptr RiverSeat -> MVar WMState -> IO ()
exitSession _ stateMVar = readMVar stateMVar >>= riverWindowManagerExitSession . currentWmManager

closeCurrentWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
closeCurrentWindow _ stateMVar = do
  modifyMVar_ stateMVar $ \state ->
    case state ^. #focusedWindow of
      Nothing -> pure state
      Just w -> pure $ state & #manageQueue <>~ riverWindowClose w

toggleFocusFloating :: Ptr RiverSeat -> MVar WMState -> IO ()
toggleFocusFloating _ stateMVar = modifyMVar_ stateMVar $ pure . execState transform
 where
  transform =
    use #focusedWindow >>= \case
      Nothing -> pure ()
      Just w -> do
        maybeWinData <- preuse (#allWindows % at w % _Just)
        fOutput <- use #focusedOutput
        outWorkmaps <- use #allOutputWorkspaces
        case (maybeWinData, B.lookup fOutput outWorkmaps) of
          (Just win, Just ws) | not (win ^. #isFullscreen) -> do
            let targetOptic
                  | view #isFloating win = #allWorkspacesTiled
                  | otherwise = #allWorkspacesFloating
            preuse (targetOptic % to (BS.lookupBs ws) % _head) >>= \case
              Just next -> do
                #focusedWindow ?= next
                #workspaceFocusHistory % at ws ?= next
                seat <- use #focusedSeat
                #manageQueue <>= riverSeatFocusWindow seat next
              Nothing -> pure ()
          _ -> pure ()

cycleWindowFocus :: Bool -> Ptr RiverSeat -> MVar WMState -> IO ()
cycleWindowFocus forward seat stateMVar = modifyMVar_ stateMVar $ pure . execState transform
 where
  transform = do
    mFwin <- use #focusedWindow
    mWs <- use focusedWorkspace
    case (mFwin, mWs) of
      (Just w, Just focusedWs) -> do
        preuse (#allWindows % at w % _Just) >>= \case
          Just win -> do
            let targetMapOptic
                  | view #isFullscreen win = #allWorkspacesFullscreen
                  | view #isFloating win = #allWorkspacesFloating
                  | otherwise = #allWorkspacesTiled

            targetMap <- use targetMapOptic

            let next = BS.lookUpNext focusedWs forward w targetMap

            nextWinData <- preuse (#allWindows % at next % _Just)
            let renderAction = case nextWinData of
                  Just nData
                    | view #isFullscreen win || view #isFloating win ->
                        riverNodePlaceTop (view #nodePtr nData)
                  _ -> pure ()

            #focusedWindow ?= next
            #workspaceFocusHistory % at focusedWs ?= next
            #renderQueue <>= renderAction
            #manageQueue <>= riverSeatFocusWindow seat next
          _ -> pure ()
      _ -> pure ()

toggleFullscreenCurrentWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
toggleFullscreenCurrentWindow _ stateMVar = modifyMVar_ stateMVar $ pure . execState transform
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
            let currentlyFullscreen = winRec ^. #isFullscreen
                currentlyFloating = winRec ^. #isFloating
            if currentlyFullscreen
              then exitFullscreen win currentlyFloating ws
              else enterFullscreen win currentlyFloating ws
            #allWindows % at win %? #isFullscreen %= not
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
    #manageQueue <>= (riverWindowExitFullscreen win >> riverWindowInformNotFullscreen win)

toggleFloatingCurrentWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
toggleFloatingCurrentWindow _ stateMVar = modifyMVar_ stateMVar $ pure . execState transform
 where
  transform = do
    mWs <- use focusedWorkspace
    mWinPtr <- use #focusedWindow
    case (mWinPtr, mWs) of
      (Just win, Just ws) -> do
        mWin <- preuse (#allWindows % at win % _Just)
        case mWin of
          Just winRec | not (winRec ^. #isPinned || winRec ^. #isFullscreen) -> do
            if view #isFloating winRec
              then exitFloating win ws
              else enterFloating win ws
            #allWindows % at win %? #isFloating %= not
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
        Just win | win ^. #isFloating && not (win ^. #isFullscreen) -> pure $ s & #allWindows % at w %? #isPinned %~ not
        _ -> pure s

toggleMaximizeWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
toggleMaximizeWindow _ stateMVar = do
  modifyMVar_ stateMVar $ \s ->
    case s ^. #focusedWindow of
      Nothing -> pure s
      Just w -> case s ^? #allWindows % at w % _Just of
        Nothing -> pure s
        Just Window{isMaximized} ->
          pure $
            s
              & (#allWindows % at w %? #isMaximized %~ not)
              & (#manageQueue <>~ if isMaximized then riverWindowInformUnmaximized w else riverWindowInformMaximized w)

cycleWindows :: Bool -> Ptr RiverSeat -> MVar WMState -> IO ()
cycleWindows forward seat stateMVar = modifyMVar_ stateMVar $ pure . execState transform
 where
  transform =
    use focusedWorkspace >>= \case
      Nothing -> pure ()
      Just focusedWs -> do
        #allWorkspacesTiled %= BS.changeSeqOrder focusedWs (cycleW forward)
        use #focusedWindow >>= \case
          Nothing -> pure ()
          Just w -> do
            tiledMap <- use #allWorkspacesTiled
            case BS.lookupA w tiledMap of
              Nothing -> pure ()
              Just workspace -> do
                let nextWin = BS.lookUpNext workspace forward w tiledMap
                #focusedWindow ?= nextWin
                #workspaceFocusHistory % at focusedWs ?= nextWin
                #manageQueue <>= riverSeatFocusWindow seat nextWin

  cycleW _ S.Empty = S.empty
  cycleW True (h S.:<| hs) = hs S.|> h
  cycleW False (hs S.:|> h) = h S.<| hs

cycleWindowSlaves :: Bool -> Ptr RiverSeat -> MVar WMState -> IO ()
cycleWindowSlaves forward seat stateMVar = modifyMVar_ stateMVar $ pure . execState transform
 where
  transform = do
    use focusedWorkspace >>= \case
      Nothing -> pure ()
      Just focusedWs -> do
        #allWorkspacesTiled %= BS.changeSeqOrder focusedWs (cycleW forward)
        use #focusedWindow >>= \case
          Nothing -> pure ()
          Just w -> do
            tiledMap <- use #allWorkspacesTiled
            let s = BS.lookupBs focusedWs tiledMap
            case S.elemIndexL w s of
              Just i | i /= 0 -> do
                let nextWin = S.index s (((if forward then i else i - 2) `mod` (length s - 1)) + 1)
                #focusedWindow ?= nextWin
                #workspaceFocusHistory % at focusedWs ?= nextWin
                #manageQueue <>= riverSeatFocusWindow seat nextWin
              _ -> pure ()

  cycleW True (h S.:<| (hs S.:|> slaveH)) = h S.<| (slaveH S.<| hs)
  cycleW False (h S.:<| (slaveH S.:<| hs)) = h S.<| (hs S.|> slaveH)
  cycleW _ hs = hs

zoomWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
zoomWindow _ stateMVar = modifyMVar_ stateMVar $ pure . execState transform
 where
  transform = do
    mWs <- use focusedWorkspace
    mWinPtr <- use #focusedWindow
    case (mWinPtr, mWs) of
      (Just currentWin, Just ws) -> do
        mWin <- preuse (#allWindows % at currentWin % _Just)
        let shouldSkip = case mWin of
              Just w -> view #isFloating w || view #isFullscreen w
              Nothing -> True
        unless shouldSkip $ #allWorkspacesTiled %= BS.changeSeqOrder ws (zoom currentWin)
      _ -> pure ()

  zoom _ S.Empty = S.empty
  zoom currentWin s@(w S.:<| ws)
    | w == currentWin = case ws of
        S.Empty -> s
        w2 S.:<| wss -> w2 S.<| (w S.<| wss)
    | otherwise = case S.elemIndexL currentWin ws of
        Nothing -> s
        Just i -> currentWin S.<| S.update i w ws

-- Does not move floating windows on another monitor
switchWorkspace :: WorkspaceID -> Ptr RiverSeat -> MVar WMState -> IO ()
switchWorkspace targetID seat stateMVar = modifyMVar_ stateMVar $ \state -> do
  let newState = execState (transform targetID) state
  broadcastState newState $ formatStatus newState
 where
  transform target = do
    fOutput <- use #focusedOutput
    outWorkmaps <- use #allOutputWorkspaces
    lastWs <- use #lastFocusedWorkspace
    case B.lookup fOutput outWorkmaps of
      Just currentWs | currentWs /= target -> do
        #allOutputWorkspaces %= B.insert fOutput target
        -- Pinned windows are moved to new workspace
        use #allWindows >>= itraverseOf_ (itraversed % filtered (^. #isPinned)) (\p _ -> #allWorkspacesFloating %= BS.move p target)

        case B.lookupR target outWorkmaps of
          Nothing -> do
            -- Hide old windows, show new windows (including pinned)
            newWins <- use (workspaceWindows target)
            currentWins <- use (workspaceWindows currentWs)
            #renderQueue <>= (mapM_ riverWindowShow newWins >> mapM_ riverWindowHide currentWins)
          Just o2 -> do
            #allOutputWorkspaces %= B.insert o2 currentWs
            -- Refullscreen old fullscreen windows on new monitor (old workspace)
            fullscreened <- use #allWorkspacesFullscreen
            forM_ (BS.lookupBs target fullscreened) $ \w ->
              do
                #allWorkspacesFullscreen %= BS.delete w
                #fullscreenQueue % at target % _Just %= (w :)
            forM_ (BS.lookupBs currentWs fullscreened) $ \w ->
              do
                #allWorkspacesFullscreen %= BS.delete w
                #fullscreenQueue % at currentWs % _Just %= (w :)

        #lastFocusedWorkspace .= currentWs
        preuse (#workspaceFocusHistory % at target % _Just) >>= \case
          Nothing -> do
            newWins <- use (workspaceWindows target)
            case newWins of
              w S.:<| _ -> do
                #focusedWindow ?= w
                #workspaceFocusHistory % at target ?= w
                #manageQueue <>= riverSeatFocusWindow seat w
              S.Empty -> do
                #focusedWindow .= Nothing
                #manageQueue <>= riverSeatClearFocus seat
          Just w -> do
            #focusedWindow ?= w
            #manageQueue <>= riverSeatFocusWindow seat w
      Just _ | lastWs /= target -> transform lastWs
      _ -> pure ()

formatStatus :: WMState -> String
formatStatus state =
  let
    windows = fmap (\i -> (i, (state ^. workspaceWindows i))) [1 .. 9]
    target = fromMaybe 1 $ B.lookup (focusedOutput state) (allOutputWorkspaces state)
    str = concat $ L.intersperse "," $ fmap (\(i, s) -> if i == target then "1" else if S.length s > 0 then "2" else "0") windows
   in
    "tags:" ++ str

focusWindow :: WindowDirection -> Ptr RiverSeat -> MVar WMState -> IO ()
focusWindow direction seat stateMVar = modifyMVar_ stateMVar $ pure . execState transform
 where
  transform = do
    mWs <- use focusedWorkspace
    mWin <- use #focusedWindow
    case (mWin, mWs) of
      (Just currentWin, Just ws) -> do
        tiled <- use (#allWorkspacesTiled % to (BS.lookupBs ws))
        case S.elemIndexL currentWin tiled of
          Just idx -> do
            geoms <- getGeometries tiled #tilingGeometry
            shiftFocus idx tiled geoms False ws
          Nothing -> do
            floating <- use (#allWorkspacesFloating % to (BS.lookupBs ws))
            case S.elemIndexL currentWin floating of
              Just idx -> do
                geoms <- getGeometries floating #floatingGeometry
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

    #focusedWindow ?= nextWin
    #workspaceFocusHistory % at ws ?= nextWin

    #manageQueue <>= riverSeatFocusWindow seat nextWin
    #manageQueue <>= riverSeatPointerWarp seat centerX centerY

    when isFloating $ do
      mNode <- preuse (#allWindows % at nextWin %? #nodePtr)
      forM_ mNode $ \node -> #renderQueue <>= riverNodePlaceTop node

swapWindow :: WindowDirection -> Ptr RiverSeat -> MVar WMState -> IO ()
swapWindow direction seat stateMVar = modifyMVar_ stateMVar $ pure . execState transform
 where
  getGeometries ptrs geoField = do
    allWins <- use #allWindows
    pure $ ptrs <&> \ptr -> fromMaybe (Rect 0 0 0 0) (allWins ^? at ptr %? geoField % _Just)
  transform = do
    mWs <- use focusedWorkspace
    mWin <- use #focusedWindow
    case (mWin, mWs) of
      (Just currentWin, Just ws) -> do
        tiled <- use (#allWorkspacesTiled % to (BS.lookupBs ws))
        case S.elemIndexL currentWin tiled of
          Nothing -> pure ()
          Just idx -> do
            geoms <- getGeometries tiled #tilingGeometry
            let nextIdx = findClosestWindow geoms direction idx
                nextWin = S.index tiled nextIdx
                rect = S.index geoms nextIdx
                centerX = rx rect + rw rect `div` 2
                centerY = ry rect + rh rect `div` 2

            #allWorkspacesTiled %= BS.changeSeqOrder ws (S.update nextIdx currentWin . S.update idx nextWin)
            #manageQueue <>= riverSeatPointerWarp seat centerX centerY
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

moveWindowToWorkspace :: WorkspaceID -> Ptr RiverSeat -> MVar WMState -> IO ()
moveWindowToWorkspace targetID seat stateMVar = modifyMVar_ stateMVar $ pure . execState transform
 where
  transform = do
    mWinPtr <- use #focusedWindow
    fOutput <- use #focusedOutput
    workmaps <- use #allOutputWorkspaces
    case (mWinPtr, B.lookup fOutput workmaps) of
      (Just win, Just currentWS)
        | currentWS /= targetID ->
            preuse (#allWindows % at win % _Just) >>= \case
              Just winRec | not (view #isPinned winRec) -> do
                moveWindowStructural win winRec
                #workspaceFocusHistory % at targetID ?= win
                #renderQueue <>= riverWindowHide win

                use (workspaceWindows currentWS) >>= \case
                  (h S.:<| _) -> do
                    #focusedWindow ?= h
                    #workspaceFocusHistory % at currentWS ?= h
                    #manageQueue <>= riverSeatFocusWindow seat h
                  S.Empty -> do
                    #focusedWindow .= Nothing
                    #workspaceFocusHistory % at currentWS .= Nothing
                    #manageQueue <>= riverSeatClearFocus seat
              _ -> pure ()
      _ -> pure ()

  moveWindowStructural win winRec
    | view #isFullscreen winRec = #allWorkspacesFullscreen %= BS.move win targetID
    | view #isFloating winRec = #allWorkspacesFloating %= BS.move win targetID
    | otherwise = #allWorkspacesTiled %= BS.move win targetID

exec :: String -> Ptr RiverSeat -> MVar WMState -> IO ()
exec command _ _ = void $ spawnCommand ("systemd-run --user --scope --slice=app.slice " ++ command)

reloadWindowManager :: FilePath -> Ptr RiverSeat -> MVar WMState -> IO ()
reloadWindowManager fp _ stateMVar = do
  state <- readMVar stateMVar
  let test = (state, state) ^. (alongside (#focusedWindow) (#opDeltaState))

  let windowsToRecord = M.fromList $ toPersistedEntry <$> (M.elems $ state ^. #allWindows)
      newPersisted = PersistedState{persistedWindows = windowsToRecord}
      toPersistedEntry w = (ident, (fromMaybe 1 $ BS.lookupA ptr ws, status))
       where
        ident = w ^. #winIdentifier
        ptr = w ^. #winPtr
        ws
          | w ^. #isFloating && w ^. #isFullscreen = state ^. #allWorkspacesFullscreen
          | w ^. #isFullscreen = state ^. #allWorkspacesFullscreen
          | w ^. #isFloating = state ^. #allWorkspacesFloating
          | otherwise = state ^. #allWorkspacesTiled
        status
          | w ^. #isFloating && w ^. #isFullscreen = FullscreenFloating
          | w ^. #isFullscreen = Fullscreen
          | w ^. #isFloating = Floating
          | otherwise = Tiled
  encodeFile fp newPersisted
  void $ spawnCommand "systemd-run --user --scope --slice=app.slice Rivermonad-reload"

dragWindow :: Ptr RiverSeat -> MVar WMState -> IO ()
dragWindow seat stateMVar = modifyMVar_ stateMVar $ pure . execState transform
 where
  transform = do
    mWin <- use #focusedWindow
    forM_ mWin $ \win -> do
      mWinRec <- preuse (#allWindows % at win % _Just)
      forM_ mWinRec $ \winRec -> do
        unless (winRec ^. #isFullscreen) $ do
          #manageQueue <>= riverSeatOpStartPointer seat
          if view #isFloating winRec
            then #opDeltaState .= Dragging
            else do
              let Rect{rx, ry} = winRec ^. #tilingGeometry % non (Rect 0 0 0 0)
              #opDeltaState .= DraggingTile
              #currentOpDelta .= (rx, ry, 0, 0)
              #allWorkspacesTiled %= BS.delete win

stopDragging :: Ptr RiverSeat -> MVar WMState -> IO ()
stopDragging seat stateMVar = modifyMVar_ stateMVar $ pure . execState finalizeDrag
 where
  finalizeDrag = do
    #manageQueue <>= riverSeatOpEnd seat
    mWin <- use #focusedWindow
    mode <- use #opDeltaState
    case (mWin, mode) of
      (Just win, Dragging) -> do
        (newX, newY, _, _) <- use #currentOpDelta
        #allWindows % at win %? #floatingGeometry %?= \r -> r{rx = newX, ry = newY}
      (Just win, DraggingTile) -> do
        fWs <- use focusedWorkspace
        let ws = fromMaybe 1 fWs

        (curX, curY, _, _) <- use #currentOpDelta
        tiledList <- use (#allWorkspacesTiled % to (BS.lookupBs ws))
        allWins <- use #allWindows

        let getCoord p = allWins ^? at p %? #tilingGeometry % _Just
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

-- stopDragging :: Ptr RiverSeat -> MVar WMState -> IO ()
-- stopDragging seat stateMVar = do
--   modifyMVar_ stateMVar $ \state -> do
--     let stop = riverSeatOpEnd seat
--     case focusedWindow state of
--       Nothing -> pure state
--       Just w -> do
--         case opDeltaState state of
--           Dragging -> do
--             let window = allWindows state M.! w
--             case floatingGeometry window of
--               Nothing -> pure state
--               Just r -> do
--                 let
--                   (x, y, _, _) = currentOpDelta state
--                   newWindows =
--                     M.insert w window{floatingGeometry = Just r{rx = x, ry = y}} (allWindows state)
--                 pure
--                   state
--                     { allWindows = newWindows
--                     , manageQueue = manageQueue state >> stop
--                     , opDeltaState = None
--                     , currentOpDelta = (0, 0, 0, 0)
--                     }
--           DraggingTile -> do
--             let
--               focusedWs = fromMaybe 1 $ state ^. focusedWorkspace
--               (currentX, currentY, _, _) = currentOpDelta state
--               currentTiled = BS.lookupBs focusedWs (allWorkspacesTiled state)
--               calcDistance (x1, y1) (x2, y2) = sqrt (fromIntegral $ (x1 - x2) ^ (2 :: Int) + (y1 - y2) ^ (2 :: Int))
--               distances :: S.Seq Double
--               distances =
--                 (\Rect{rx, ry} -> calcDistance (rx, ry) (currentX, currentY))
--                   . (fromMaybe (Rect 0 0 0 0))
--                   . tilingGeometry
--                   . (allWindows state M.!)
--                   <$> currentTiled
--               index = case distances of
--                 S.Empty -> 0
--                 h S.:<| t -> fst $ S.foldlWithIndex (\(oldI, old) i newD -> if newD < old then (i + 1, newD) else (oldI, old)) (0, h) t
--               newTiled = BS.insertByIndex focusedWs w (fromIntegral index) (allWorkspacesTiled state)
--             pure
--               state
--                 { manageQueue = manageQueue state >> stop
--                 , opDeltaState = None
--                 , allWorkspacesTiled = newTiled
--                 , currentOpDelta = (0, 0, 0, 0)
--                 }
--           _ -> pure state{manageQueue = manageQueue state >> stop}

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
