module Handlers.Seat (mkSeatHandler) where

import Control.Concurrent.MVar
import Control.Monad (forM_, msum, when)
import Control.Monad.State
import Data.Bimap qualified as B
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe, listToMaybe)
import Optics.Core
import Optics.State
import Optics.State.Operators
import Protocols.Generated
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.Connection

mkSeatHandler :: MVar WMState -> RiverSeatV1Handlers
mkSeatHandler mvar =
  RiverSeatV1Handlers
    { onRiverSeatV1OpDelta = opDelta mvar
    , onRiverSeatV1OpRelease = \_ -> pure ()
    , onRiverSeatV1WindowInteraction = windowInteraction mvar
    , onRiverSeatV1ShellSurfaceInteraction = \_ _ -> pure ()
    , onRiverSeatV1PointerEnter = \_ _ -> pure ()
    , onRiverSeatV1PointerLeave = \_ -> pure ()
    , onRiverSeatV1PointerPosition = pointerPosition mvar
    , onRiverSeatV1WlSeat = wlSeat mvar
    , onRiverSeatV1Removed = removed mvar
    }

wlSeat :: MVar WMState -> Object RiverSeatV1 -> Word32 -> W ()
wlSeat mvar seat name = do
  modifyMVarW_ mvar $ \s -> do
    pure $ s & #allSeats % at seat %? #seatWlSeat .~ name

removed :: MVar WMState -> Object RiverSeatV1 -> W ()
removed mvar seat = do
  modifyMVarW_ mvar $ \s -> do
    traverseOf_ (#allSeats % at seat %? #seatXkbBinds % traversed) riverXkbBindingV1Destroy s
    traverseOf_ (#allSeats % at seat %? #seatPtrBinds % traversed) riverPointerBindingV1Destroy s
    riverSeatV1Destroy seat
    execStateT transform s
 where
  transform = do
    #allSeats %= M.delete seat
    newFocusedSeat <- use (#allSeats % to ((fromMaybe nonObject) . listToMaybe . (fst <$>) . M.toList))
    #focusedSeat %= (\oldS -> if oldS == seat then newFocusedSeat else oldS)

windowInteraction :: MVar WMState -> Object RiverSeatV1 -> Object RiverWindowV1 -> W ()
windowInteraction mvar _ win = do
  modifyMVarW_ mvar $ pure . execState transform
 where
  transform = do
    use (pairOfGetter #focusedWin (#allWindows % at win)) >>= \case
      (Just fWin, _) | fWin == win -> pure ()
      (_, Just winRec) -> do
        tiled <- use #allWorkspacesTiled
        floating <- use #allWorkspacesFloating
        full <- use #allWorkspacesFullscreen
        forM_ (msum $ BS.lookupA win <$> [tiled, floating, full]) $ \ws -> do
          setFocusedWindowAndHistory ws win
          when (winRec ^. #winFloat) $ #renderQueue >>>= riverNodeV1PlaceTop (winRec ^. #winNodeObj)

          oToW <- use #allOutputWorkspaces
          oldO <- use #focusedOut
          case B.lookupR ws oToW of
            Just o | o /= oldO -> do
              #focusedOut .= o
            _ -> pure ()
      _ -> pure ()

opDelta :: MVar WMState -> Object RiverSeatV1 -> Int32 -> Int32 -> W ()
opDelta mvar _ dx dy = do
  modifyMVarW_ mvar $ pure . execState transform
 where
  transform = do
    use (pairOfGetter #focusedWin #opDeltaState) >>= \case
      (Just win, mode) -> do
        mWinRec <- use (#allWindows % at win)
        forM_ mWinRec $ \winRec -> do
          case mode of
            Dragging -> handleDrag winRec
            Resizing edge -> handleResize win winRec edge
            ResizingTile -> handleTileResize
            DraggingTile -> handleTileDrag winRec
            None -> pure ()
      _ -> pure ()

  handleDrag win = forM_ (view #winFloatGeo win) $ \Rect{rx, ry} -> do
    moutGeom <- use focusedOutputGeom
    forM_ moutGeom $ \outGeom -> do
      let (newX, newY) = (rx + dx, ry + dy)
      #renderQueue >>>= riverNodeV1SetPosition (view #winNodeObj win) (newX + outGeom ^. #rx) (newY + outGeom ^. #ry)
      #currentOpDelta .= (newX, newY, 0, 0)

  handleTileResize = do
    (oldDx, _, _, _) <- use #currentOpDelta
    ws <- use (focusedWorkspace % non 1)
    preuse (focusedOutputGeom %? #rw) >>= \case
      Nothing -> pure ()
      Just outW -> do
        let frac = fromIntegral (dx - oldDx) / fromIntegral outW
        #workspaceLayouts % at ws %?= \layout -> fromMaybe layout (handleSomeMsg layout $ SomeMessage $ IncMasterFrac frac)
        #currentOpDelta .= (dx, 0, 0, 0)

  handleTileDrag win = forM_ (view #winTileGeo win) $ \Rect{rx, ry} -> do
    let newX = rx + dx
        newY = ry + dy
    #renderQueue >>>= riverNodeV1SetPosition (win ^. #winNodeObj) newX newY
    #currentOpDelta .= (newX, newY, 0, 0)

  handleResize winPtr winRec e =
    forM_ (view #winFloatGeo winRec) $ \Rect{rx, ry, rw, rh} -> do
      let (minW, minH, _, _) = view #winDimHint winRec
          minminW = max minW 15
          minminH = max minH 15
          nWm = max (rw - dx) minminW
          nWp = max (rw + dx) minminW
          nHm = max (rh - dy) minminH
          nHp = max (rh + dy) minminH
          nX = min (rx + dx) (rw + rx - minminW)
          nY = min (ry + dy) (ry + rh - minminH)

          (w, h, x, y)
            | edgeTop e = (rw, nHm, rx, nY)
            | edgeBottom e = (rw, nHp, rx, ry)
            | edgeRight e = (nWp, rh, rx, ry)
            | edgeLeft e = (nWm, rh, nX, ry)
            | edgeTopLeft e = (nWm, nHm, nX, nY)
            | edgeTopRight e = (nWp, nHm, rx, nY)
            | edgeBottomLeft e = (nWm, nHp, nX, ry)
            | edgeBottomRight e = (nWp, nHp, rx, ry)
            | otherwise = (rw, rh, rx, ry)

      #manageQueue >>>= riverWindowV1ProposeDimensions winPtr w h
      #renderQueue >>>= riverNodeV1SetPosition (winRec ^. #winNodeObj) x y
      #currentOpDelta .= (x, y, w, h)

pointerPosition :: MVar WMState -> Object RiverSeatV1 -> Int32 -> Int32 -> W ()
pointerPosition mvar _ x y = do
  modifyMVarW_ mvar $ \s -> pure s{cursorPosition = (x, y)}
