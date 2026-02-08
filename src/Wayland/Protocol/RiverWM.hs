module Wayland.Protocol.RiverWM where

-- import Data.Text qualified as T

import Data.IORef
import Data.Map qualified as M
import Foreign
import Layout
import Types
import Wayland.Client
import Wayland.Protocol.ImportedFunctions

foreign export ccall "hs_on_new_window"
  hs_on_new_window :: Ptr () -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_on_new_output"
  hs_on_new_output :: Ptr () -> Ptr RiverOutput -> IO ()
foreign export ccall "hs_on_new_seat"
  hs_on_new_seat :: Ptr () -> Ptr RiverSeat -> IO ()
foreign export ccall "hs_manage_start"
  hs_manage_start :: Ptr () -> Ptr RiverWMManager -> IO ()
foreign export ccall "hs_render_start"
  hs_render_start :: Ptr () -> Ptr RiverWMManager -> IO ()
foreign export ccall "hs_output_position"
  hs_output_position :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
foreign export ccall "hs_output_dimensions"
  hs_output_dimensions :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
foreign export ccall "hs_pointer_enter"
  hs_pointer_enter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()

hs_pointer_enter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hs_pointer_enter dataPtr seat win = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  let focusWin = river_seat_v1_focus_window seat win
  modifyIORef stateIORef $ \state -> state{manageQueue = manageQueue state >> focusWin}

hs_output_dimensions :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
hs_output_dimensions dataPtr output width height = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateIORef $ \state -> do
    let oldOutputs = allOutputs state
    case M.lookup output oldOutputs of
      Nothing -> state
      Just o -> do
        let updatedOutput = o{outWidth = width, outHeight = height}
            newOutputs = M.insert output updatedOutput oldOutputs
        state{allOutputs = newOutputs}

hs_output_position :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
hs_output_position dataPtr output x y = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateIORef $ \state -> do
    let oldOutputs = allOutputs state
    case M.lookup output oldOutputs of
      Nothing -> state
      Just o -> do
        let updatedOutput = o{outX = x, outY = y}
            newOutputs = M.insert output updatedOutput oldOutputs
        state{allOutputs = newOutputs}

hs_on_new_window :: Ptr () -> Ptr RiverWindow -> IO ()
hs_on_new_window dataPtr win = do
  node <- river_window_v1_get_node win
  let w =
        Window
          { winPtr = win
          , nodePtr = node
          , isFloating = False
          , isFullscreen = False
          }
  _ <- wl_proxy_add_listener (castPtr win) get_river_window_listener dataPtr
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateIORef $ \state -> do
    let newWindowsList = M.insert win w (allWindows state)
        newManageQueue = manageQueue state >> (startupApplyManage win)
    state{allWindows = newWindowsList, manageQueue = newManageQueue, focusedWindow = win}

hs_on_new_seat :: Ptr () -> Ptr RiverSeat -> IO ()
hs_on_new_seat dataPtr seat = do
  _ <- wl_proxy_add_listener (castPtr seat) get_river_seat_listener dataPtr
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateIORef $ \state -> state{currentSeat = seat}

hs_on_new_output :: Ptr () -> Ptr RiverOutput -> IO ()
hs_on_new_output dataPtr output = do
  let o = Output output 0 0 0 0
  _ <- wl_proxy_add_listener (castPtr output) get_river_output_listener dataPtr
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateMVar $ \state -> do
    let newOutputsList = M.insert output o (allOutputs state)
    state{allOutputs = newOutputsList, focusedOutput = output}

hs_manage_start :: Ptr () -> Ptr RiverWMManager -> IO ()
hs_manage_start dataPtr wmManager = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  manageQueue state
  renderActions <- applyLayout state
  river_window_manager_v1_manage_finish wmManager
  writeIORef
    stateIORef
    state
      { manageQueue = return ()
      , renderQueue = renderQueue state >> renderActions
      }

hs_render_start :: Ptr () -> Ptr RiverWMManager -> IO ()
hs_render_start dataPtr wmManager = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  renderQueue state
  river_window_manager_v1_render_finish wmManager
  writeIORef stateIORef state{renderQueue = return ()}

startupApplyManage :: Ptr RiverWindow -> IO ()
startupApplyManage w = do
  let use_ssd = river_window_v1_use_ssd w
      set_tiled = river_window_v1_set_tiled w edgeBottom
  set_tiled >> use_ssd

startupApplyRender :: Ptr RiverWindow -> Ptr RiverNode -> IO ()
startupApplyRender _ _ = pure ()
