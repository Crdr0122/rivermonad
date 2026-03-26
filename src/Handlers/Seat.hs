module Handlers.Seat where

import Control.Concurrent.MVar
import Control.Monad (forM_, msum, when)
import Control.Monad.State hiding (state)
import Data.Bimap qualified as B
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C
import Optics.Core
import Optics.State
import Optics.State.Operators
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.ImportedFunctions

foreign export ccall "hs_seat_pointer_enter"
  hsSeatPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()

foreign export ccall "hs_seat_window_interaction"
  hsSeatWindowInteraction :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()

foreign export ccall "hs_seat_op_delta"
  hsSeatOpDelta :: Ptr () -> Ptr RiverSeat -> CInt -> CInt -> IO ()

foreign export ccall "hs_seat_op_release"
  hsSeatOpRelease :: Ptr () -> Ptr RiverSeat -> IO ()

foreign export ccall "hs_seat_pointer_position"
  hsSeatPointerPosition :: Ptr () -> Ptr RiverSeat -> CInt -> CInt -> IO ()

foreign export ccall "hs_seat_removed"
  hsSeatRemoved :: Ptr () -> Ptr RiverSeat -> IO ()

hsSeatRemoved :: Ptr () -> Ptr RiverSeat -> IO ()
hsSeatRemoved dataPtr seat = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> do
    mapM_ riverXkbBindingDestroy $ state ^. #seatXkbBindings % at seat % non []
    mapM_ riverPointerBindingDestroy $ state ^. #seatPointerBindings % at seat % non []
    riverSeatDestroy seat
    pure $
      state
        & (#seatPointerBindings %~ M.delete seat)
        & (#seatXkbBindings %~ M.delete seat)
        & (#focusedSeat %~ (\oldS -> if oldS == seat then nullPtr else oldS))

hsSeatPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hsSeatPointerEnter dataPtr _ _ = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> pure state

hsSeatWindowInteraction :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hsSeatWindowInteraction dataPtr seat win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
 where
  transform =
    use #focusedWindow >>= \case
      Just fWin | fWin == win -> #manageQueue <>= riverSeatFocusWindow seat win
      _ -> do
        mWinRec <- use (#allWindows % at win)
        forM_ mWinRec $ \winRec -> do
          tiled <- use #allWorkspacesTiled
          floating <- use #allWorkspacesFloating
          full <- use #allWorkspacesFullscreen
          forM_ (msum $ BS.lookupA win <$> [tiled, floating, full]) $ \ws -> do
            #focusedWindow ?= win
            #workspaceFocusHistory %= M.insert ws win
            #manageQueue <>= riverSeatFocusWindow seat win
            when (winRec ^. #isFloating) $ #renderQueue <>= riverNodePlaceTop (winRec ^. #nodePtr)

            oToW <- use #allOutputWorkspaces
            oldO <- use #focusedOutput
            case B.lookupR ws oToW of
              Just o | o /= oldO -> do
                #focusedOutput .= o
                preuse (#allOutputs % at o %? #outLayerShell) >>= \case
                  Nothing -> pure ()
                  Just oRec -> #manageQueue <>= riverLayerShellOutputSetDefault oRec
              _ -> pure ()

hsSeatOpDelta :: Ptr () -> Ptr RiverSeat -> CInt -> CInt -> IO ()
hsSeatOpDelta dataPtr _ dx dy = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
 where
  transform =
    use (pairOfGetter #focusedWindow #opDeltaState) >>= \case
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

  handleDrag win = forM_ (view #floatingGeometry win) $ \Rect{rx, ry} -> do
    moutGeom <- use focusedOutputGeom
    forM_ moutGeom $ \outGeom -> do
      let (newX, newY) = (rx + dx, ry + dy)
      #renderQueue <>= riverNodeSetPosition (view #nodePtr win) (newX + outGeom ^. #rx) (newY + outGeom ^. #ry)
      #currentOpDelta .= (newX, newY, 0, 0)

  handleTileResize = do
    (oldDx, _, _, _) <- use #currentOpDelta
    ws <- use focusedWorkspace
    preuse (focusedOutputGeom %? #rw) >>= \case
      Nothing -> pure ()
      Just outW -> do
        #workspaceLayouts % at (fromMaybe 1 ws) %?= \layout -> fromMaybe layout (handleSomeMsg layout $ SomeMessage $ IncMasterFrac (fromIntegral (dx - oldDx) / fromIntegral outW))
        #currentOpDelta .= (dx, 0, 0, 0)

  handleTileDrag win = forM_ (view #tilingGeometry win) $ \Rect{rx, ry} -> do
    let newX = rx + dx
        newY = ry + dy
    #renderQueue <>= riverNodeSetPosition (win ^. #nodePtr) newX newY
    #currentOpDelta .= (newX, newY, 0, 0)

  handleResize winPtr winRec e =
    forM_ (view #floatingGeometry winRec) $ \Rect{rx, ry, rw, rh} -> do
      let (minW, minH, _, _) = view #dimensionsHint winRec
          minminW = max minW 15
          minminH = max minH 15
          nWm = max (rw - dx) minminW
          nWp = max (rw + dx) minminW
          nHm = max (rh - dy) minminH
          nHp = max (rh + dy) minminH
          nX = min (rx + dx) (rw + rx - minminW)
          nY = min (ry + dy) (ry + rh - minminH)

          (w, h, x, y)
            | e == edgeTop = (rw, nHm, rx, nY)
            | e == edgeBottom = (rw, nHp, rx, ry)
            | e == edgeRight = (nWp, rh, rx, ry)
            | e == edgeLeft = (nWm, rh, nX, ry)
            | e == edgeTopLeft = (nWm, nHm, nX, nY)
            | e == edgeTopRight = (nWp, nHm, rx, nY)
            | e == edgeBottomLeft = (nWm, nHp, nX, ry)
            | e == edgeBottomRight = (nWp, nHp, rx, ry)
            | otherwise = (rw, rh, rx, ry)

      #manageQueue <>= riverWindowProposeDimensions winPtr w h
      #renderQueue <>= riverNodeSetPosition (winRec ^. #nodePtr) x y
      #currentOpDelta .= (x, y, w, h)

hsSeatOpRelease :: Ptr () -> Ptr RiverSeat -> IO ()
hsSeatOpRelease _ _ = pure ()

hsSeatPointerPosition :: Ptr () -> Ptr RiverSeat -> CInt -> CInt -> IO ()
hsSeatPointerPosition dataPtr _ x y = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> pure state{cursorPosition = (x, y)}
