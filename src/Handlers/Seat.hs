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
  case opDeltaState state of
    None -> pure ()
    Dragging -> do
      case focusedWindow of
        Nothing -> pure ()
        Just w -> do
          let
            Window{floatingGeometry, nodePtr} = allWindows M.! w
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
                  , currentOpDelta = (dx, dy)
                  }
    Resizing -> do
      case focusedWindow of
        Nothing -> pure ()
        Just w -> do
          let
            Window{floatingGeometry} = allWindows M.! w
          case floatingGeometry of
            Nothing -> pure ()
            Just Rect{rw, rh} -> do
              let
                (newW, newH) = (max (rw + dx) 15, max (rh + dy) 15)
                resize =
                  riverWindowProposeDimensions w newW newH
              writeIORef
                stateIORef
                state
                  { manageQueue = manageQueue state >> resize
                  , currentOpDelta = (dx, dy)
                  }

hsOpRelease :: Ptr () -> Ptr RiverSeat -> IO ()
hsOpRelease _ _ = pure ()
