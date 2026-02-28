module Handlers.Seat where

import Control.Monad (when)
import Data.IORef
import Data.Map.Strict qualified as M
import Foreign
import Foreign.C
import Types
import Wayland.ImportedFunctions

foreign export ccall "hs_pointer_enter"
  hsPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()

foreign export ccall "hs_window_interaction"
  hsWindowInteraction :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()

foreign export ccall "hs_op_delta"
  hsOpDelta :: Ptr () -> Ptr RiverSeat -> CInt -> CInt -> IO ()

foreign export ccall "hs_op_release"
  hsOpRelease :: Ptr () -> Ptr RiverSeat -> IO ()

foreign export ccall "hs_pointer_position"
  hsPointerPosition :: Ptr () -> Ptr RiverSeat -> CInt -> CInt -> IO ()

hsPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hsPointerEnter dataPtr seat win = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  writeIORef
    stateIORef
    state
      { focusedWindow = Just (win)
      , manageQueue = manageQueue state >> riverSeatFocusWindow seat win
      }

hsWindowInteraction :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hsWindowInteraction dataPtr _ win = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  let Window{nodePtr, isFloating} = (allWindows state) M.! win
  when isFloating $ do
    writeIORef
      stateIORef
      state
        { focusedWindow = Just (win)
        , manageQueue = manageQueue state >> riverNodePlaceTop nodePtr
        }

hsOpDelta :: Ptr () -> Ptr RiverSeat -> CInt -> CInt -> IO ()
hsOpDelta dataPtr _ dx dy = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state@WMState
    { allOutputs
    , focusedOutput
    , focusedWindow
    , allWindows
    } <-
    readIORef stateIORef
  case focusedWindow of
    Nothing -> pure ()
    Just win -> do
      case opDeltaState state of
        None -> pure ()
        Dragging -> do
          let Window{floatingGeometry, nodePtr} = allWindows M.! win
          case floatingGeometry of
            Nothing -> pure ()
            Just Rect{rx, ry} -> do
              let
                Output{outX, outY} = allOutputs M.! focusedOutput
                (newX, newY) = (rx + dx, ry + dy)
                reposition =
                  riverNodeSetPosition nodePtr (newX + outX) (newY + outY)
              writeIORef
                stateIORef
                state
                  { renderQueue = renderQueue state >> reposition
                  , currentOpDelta = (newX, newY, 0, 0)
                  }
        Resizing edge -> do
          let Window{floatingGeometry, nodePtr} = allWindows M.! win
          case floatingGeometry of
            Nothing -> pure ()
            Just Rect{rx, ry, rw, rh} -> do
              let (w, h, x, y)
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
                    newX = min (rx + dx) (rw + rx - 15)
                    newY = min (ry + dy) (ry + rh - 15)
                    newWidthMinus = max (rw - dx) 15
                    newWidthPlus = max (rw + dx) 15
                    newHeightMinus = max (rh - dy) 15
                    newHeightPlus = max (rh + dy) 15
              writeIORef
                stateIORef
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
                (focusedWorkspace state)
                (fromIntegral (dx - oldDx) / fromIntegral outWidth)
                (workspaceRatios state)
          writeIORef
            stateIORef
            state
              { workspaceRatios = newWorkspaceRatios
              , currentOpDelta = (dx, 0, 0, 0)
              }

hsOpRelease :: Ptr () -> Ptr RiverSeat -> IO ()
hsOpRelease _ _ = pure ()

hsPointerPosition :: Ptr () -> Ptr RiverSeat -> CInt -> CInt -> IO ()
hsPointerPosition dataPtr _ x y = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateIORef $ \state ->
    state{cursorPosition = (x, y)}
