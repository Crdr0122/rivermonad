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
  let window = (allWindows state) M.! win
      node = nodePtr window
  when (isFloating window) $ do
    writeIORef
      stateIORef
      state
        { focusedWindow = Just (win)
        , manageQueue = manageQueue state >> riverNodePlaceTop node
        }

hsOpDelta :: Ptr () -> Ptr RiverSeat -> CInt -> CInt -> IO ()
hsOpDelta dataPtr _ dx dy = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  when (isDraggingWindow state) $ do
    case focusedWindow state of
      Nothing -> pure ()
      Just w -> do
        let
          window = allWindows state M.! w
        case floatingGeometry window of
          Nothing -> pure ()
          Just Rect{rx, ry} -> do
            let
              Output{outX, outY} = allOutputs state M.! focusedOutput state
              (newX, newY) = (rx + fromIntegral dx, ry + fromIntegral dy)
              reposition =
                riverNodeSetPosition (nodePtr window) (fromIntegral $ newX + outX) (fromIntegral $ newY + outY)
            writeIORef
              stateIORef
              state
                { renderQueue = renderQueue state >> reposition
                , currentOpDelta = (fromIntegral dx, fromIntegral dy)
                }

hsOpRelease :: Ptr () -> Ptr RiverSeat -> IO ()
hsOpRelease dataPtr _ = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  case focusedWindow state of
    Nothing -> pure ()
    Just w -> do
      let
        window = allWindows state M.! w
      case floatingGeometry window of
        Nothing -> pure ()
        Just Rect{rx, ry, rw, rh} -> do
          let
            (dx, dy) = currentOpDelta state
            (newX, newY) = (rx + dx, ry + dy)
            newWindows = M.insert w window{floatingGeometry = Just Rect{rx = newX, ry = newY, rw, rh}} (allWindows state)
          writeIORef stateIORef state{allWindows = newWindows}
