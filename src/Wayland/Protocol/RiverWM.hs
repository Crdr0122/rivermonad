module Wayland.Protocol.RiverWM where

import Control.Concurrent.MVar

-- import Data.Text qualified as T

import Data.Bits ((.|.))
import Data.Map qualified as M
import Foreign
import Types
import Wayland.Client
import Wayland.Protocol.ImportedFunctions

foreign export ccall "hs_on_new_window"
  hs_on_new_window :: Ptr () -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_on_new_output"
  hs_on_new_output :: Ptr () -> Ptr RiverOutput -> IO ()
foreign export ccall "hs_manage_start"
  hs_manage_start :: Ptr () -> Ptr RiverWMManager -> IO ()
foreign export ccall "hs_render_start"
  hs_render_start :: Ptr () -> Ptr RiverWMManager -> IO ()
foreign export ccall "hs_output_position"
  hs_output_position :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
foreign export ccall "hs_output_dimensions"
  hs_output_dimensions :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()

hs_output_dimensions :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
hs_output_dimensions dataPtr output width height = do
  return ()

hs_output_position :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
hs_output_position dataPtr output x y = do
  return ()

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
  windowPtr <- newStablePtr w
  -- Add window listener
  _ <- wl_proxy_add_listener (castPtr win) get_river_window_listener (castStablePtrToPtr windowPtr)
  state <- deRefStablePtr (castPtrToStablePtr dataPtr)
  let windowsMVar = allWindows state
  modifyMVar_ windowsMVar (pure . (M.insert win w))

  let outputsMVar = allOutputs state
      manageMVar = manageQueue state
  -- renderMVar = renderQueue state
  outputs <- readMVar outputsMVar
  modifyMVar_ manageMVar (startupApplyManage win)

hs_on_new_output :: Ptr () -> Ptr RiverOutput -> IO ()
hs_on_new_output dataPtr output = do
  let o = Output output 0 0 0 0
  outputPtr <- newStablePtr o
  _ <- wl_proxy_add_listener (castPtr output) get_river_output_listener (castStablePtrToPtr outputPtr)
  -- Add output into list of outputs
  state <- deRefStablePtr (castPtrToStablePtr dataPtr)
  let outputsMVar = allOutputs state
  modifyMVar_ outputsMVar (pure . (M.insert output o))

hs_manage_start :: Ptr () -> Ptr RiverWMManager -> IO ()
hs_manage_start dataPtr wmManager = do
  state <- deRefStablePtr (castPtrToStablePtr dataPtr)
  let actionsMVar = manageQueue state
  manageActions <- swapMVar actionsMVar (pure ())
  manageActions
  river_window_manager_v1_manage_finish wmManager

hs_render_start :: Ptr () -> Ptr RiverWMManager -> IO ()
hs_render_start dataPtr wmManager = do
  state <- deRefStablePtr (castPtrToStablePtr dataPtr)
  let actionsMVar = renderQueue state
  renderActions <- swapMVar actionsMVar (pure ())
  renderActions
  river_window_manager_v1_render_finish wmManager

startupApplyManage :: Ptr RiverWindow -> IO () -> IO (IO ())
startupApplyManage w a = do
  let use_ssd = river_window_v1_use_ssd w
      set_tiled = river_window_v1_set_tiled w edgeBottom
      propose_dimensions = river_window_v1_propose_dimensions w 0 0
  pure (a >> set_tiled >> use_ssd >> propose_dimensions)

startupApplyRender :: Ptr RiverWindow -> Ptr RiverNode -> IO () -> IO (IO ())
startupApplyRender w n a = do
  pure a
