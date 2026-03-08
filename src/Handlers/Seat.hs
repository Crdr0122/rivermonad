module Handlers.Seat where

import Control.Concurrent.MVar
import Control.Monad (when)
import Data.Bimap qualified as B
import Data.Map.Strict qualified as M
import Foreign
import Foreign.C
import Types
import Utils.BiSeqMap qualified as BS
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

hsSeatPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hsSeatPointerEnter dataPtr seat win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let window = allWindows state M.! win
        workspace
          | isFullscreen window = BS.findA win (allWorkspacesFullscreen state)
          | isFloating window = BS.findA win (allWorkspacesFloating state)
          | otherwise = BS.findA win (allWorkspacesTiled state)
        output = case B.lookupR workspace (allOutputWorkspaces state) of
          Nothing -> focusedOutput state
          Just o -> o
    pure
      state
        { manageQueue = manageQueue state >> riverSeatFocusWindow seat win
        , focusedWindow = Just (win)
        , focusedOutput = output
        }

hsSeatWindowInteraction :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hsSeatWindowInteraction dataPtr seat win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let Window{nodePtr, isFloating} = (allWindows state) M.! win
    pure
      state
        { focusedWindow = Just (win)
        , manageQueue = manageQueue state >> when isFloating (riverNodePlaceTop nodePtr) >> riverSeatFocusWindow seat win
        }

hsSeatOpDelta :: Ptr () -> Ptr RiverSeat -> CInt -> CInt -> IO ()
hsSeatOpDelta dataPtr _ dx dy = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $
    \state@WMState
       { allOutputs
       , focusedOutput
       , focusedWindow
       , allWindows
       , allOutputWorkspaces
       } -> do
        case focusedWindow of
          Nothing -> pure state
          Just win -> do
            case opDeltaState state of
              None -> pure state
              Dragging -> do
                let Window{floatingGeometry, nodePtr} = allWindows M.! win
                case floatingGeometry of
                  Nothing -> pure state
                  Just Rect{rx, ry} -> do
                    let
                      Output{outX, outY} = allOutputs M.! focusedOutput
                      (newX, newY) = (rx + dx, ry + dy)
                      reposition =
                        riverNodeSetPosition nodePtr (newX + outX) (newY + outY)
                    pure
                      state
                        { renderQueue = renderQueue state >> reposition
                        , currentOpDelta = (newX, newY, 0, 0)
                        }
              Resizing edge -> do
                let Window{floatingGeometry, nodePtr, dimensionsHint} = allWindows M.! win
                case floatingGeometry of
                  Nothing -> pure state
                  Just Rect{rx, ry, rw, rh} -> do
                    let (minW, minH, _, _) = dimensionsHint
                        (w, h, x, y)
                          | edge == edgeTop = (rw, newHeightMinus, rx, newY)
                          | edge == edgeBottom = (rw, newHeightPlus, rx, ry)
                          | edge == edgeRight = (newWidthPlus, rh, rx, ry)
                          | edge == edgeLeft = (newWidthMinus, rh, newX, ry)
                          | edge == edgeTopLeft = (newWidthMinus, newHeightMinus, newX, newY)
                          | edge == edgeTopRight = (newWidthPlus, newHeightMinus, rx, newY)
                          | edge == edgeBottomLeft = (newWidthMinus, newHeightPlus, newX, ry)
                          | edge == edgeBottomRight = (newWidthPlus, newHeightPlus, rx, ry)
                          | otherwise = (rw, rh, rx, ry)
                         where
                          minminW = max minW 15
                          minminH = max minH 15
                          newX = min (rx + dx) (rw + rx - minminW)
                          newY = min (ry + dy) (ry + rh - minminH)
                          newWidthMinus = max (rw - dx) minminW
                          newWidthPlus = max (rw + dx) minminW
                          newHeightMinus = max (rh - dy) minminH
                          newHeightPlus = max (rh + dy) minminH
                    pure
                      state
                        { manageQueue = manageQueue state >> riverWindowProposeDimensions win w h
                        , renderQueue = renderQueue state >> riverNodeSetPosition nodePtr x y
                        , currentOpDelta = (x, y, w, h)
                        }
              ResizingTile -> do
                let
                  Output{outWidth} = allOutputs M.! focusedOutput
                  (oldDx, _, _, _) = currentOpDelta state
                  newWorkspaceRatios =
                    M.insertWith
                      (\n o -> let m = o + n in if m > 0.20 && m < 0.80 then m else o)
                      (allOutputWorkspaces B.! focusedOutput)
                      (fromIntegral (dx - oldDx) / fromIntegral outWidth)
                      (workspaceRatios state)
                pure
                  state
                    { workspaceRatios = newWorkspaceRatios
                    , currentOpDelta = (dx, 0, 0, 0)
                    }
              DraggingTile w -> do
                let Window{tilingGeometry, nodePtr} = (allWindows M.! w)
                case tilingGeometry of
                  Nothing -> pure state
                  Just Rect{rx = ox, ry = oy} -> do
                    let
                      newX = ox + dx
                      newY = oy + dy
                    pure
                      state
                        { renderQueue = renderQueue state >> riverNodeSetPosition nodePtr newX newY
                        , currentOpDelta = (newX, newY, 0, 0)
                        }

hsSeatOpRelease :: Ptr () -> Ptr RiverSeat -> IO ()
hsSeatOpRelease _ _ = pure ()

hsSeatPointerPosition :: Ptr () -> Ptr RiverSeat -> CInt -> CInt -> IO ()
hsSeatPointerPosition dataPtr _ x y = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state ->
    pure state{cursorPosition = (x, y)}
