{-# LANGUAGE CApiFFI #-}

module Wayland.Protocol.ImportedFunctions where

import Foreign
import Foreign.C
import Types

foreign import ccall unsafe "get_river" get_river :: Ptr ()
foreign import ccall unsafe "get_river_wm_listener" get_river_wm_listener :: Ptr ()
foreign import ccall unsafe "get_river_window_listener" get_river_window_listener :: Ptr ()
foreign import ccall unsafe "get_river_output_listener" get_river_output_listener :: Ptr ()

foreign import capi "river-wm.h river_window_manager_v1_manage_finish"
  river_window_manager_v1_manage_finish :: Ptr RiverWMManager -> IO ()
foreign import capi "river-wm.h river_window_manager_v1_render_finish"
  river_window_manager_v1_render_finish :: Ptr RiverWMManager -> IO ()

foreign import capi "river-wm.h river_window_v1_destroy"
  river_window_v1_destroy :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_close"
  river_window_v1_close :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_get_node"
  river_window_v1_get_node :: Ptr RiverWindow -> IO (Ptr RiverNode)
foreign import capi "river-wm.h river_window_v1_propose_dimensions"
  river_window_v1_propose_dimensions :: Ptr RiverWindow -> CInt -> CInt -> IO ()
foreign import capi "river-wm.h river_window_v1_hide"
  river_window_v1_hide :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_show"
  river_window_v1_show :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_use_csd"
  river_window_v1_use_csd :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_use_ssd"
  river_window_v1_use_ssd :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_set_borders"
  river_window_v1_set_borders :: Ptr RiverWindow -> Word32 -> CInt -> RiverEdge -> Word32 -> Word32 -> Word32 -> IO ()
foreign import capi "river-wm.h river_window_v1_set_tiled"
  river_window_v1_set_tiled :: Ptr RiverWindow -> RiverEdge -> IO ()
foreign import capi "river-wm.h river_window_v1_get_decoration_above"
  river_window_v1_get_decoration_above :: Ptr RiverWindow -> Ptr () -> IO ()
foreign import capi "river-wm.h river_window_v1_get_decoration_below"
  river_window_v1_get_decoration_below :: Ptr RiverWindow -> Ptr () -> IO ()
foreign import capi "river-wm.h river_window_v1_inform_resize_start"
  river_window_v1_inform_resize_start :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_inform_resize_end"
  river_window_v1_inform_resize_end :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_set_capabilities"
  river_window_v1_inform_set_capabilities :: Ptr RiverWindow -> Word32 -> IO ()
foreign import capi "river-wm.h river_window_v1_inform_maximized"
  river_window_v1_inform_informed_maximized :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_inform_unmaximized"
  river_window_v1_inform_informed_unmaximized :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_inform_fullscreen"
  river_window_v1_inform_informed_fullscreen :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_inform_not_fullscreen"
  river_window_v1_inform_informed_not_fullscreen :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_fullscreen"
  river_window_v1_fullscreen :: Ptr RiverWindow -> Ptr RiverOutput -> IO ()
foreign import capi "river-wm.h river_window_v1_exit_fullscreen"
  river_window_v1_exit_fullscreen :: Ptr RiverWindow -> IO ()
foreign import capi "river-wm.h river_window_v1_set_clip_box"
  river_window_v1_set_clip_box :: Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import capi "river-wm.h river_window_v1_set_content_clip_box"
  river_window_v1_set_content_clip_box :: Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()

foreign import capi "river-wm.h river_node_v1_destroy"
  river_node_v1_destroy :: Ptr RiverNode -> IO ()
foreign import capi "river-wm.h river_node_v1_set_position"
  river_node_v1_set_position :: Ptr RiverNode -> Word32 -> Word32 -> IO ()
foreign import capi "river-wm.h river_node_v1_place_top"
  river_node_v1_place_top :: Ptr RiverNode -> IO ()
foreign import capi "river-wm.h river_node_v1_place_bottom"
  river_node_v1_place_bottom :: Ptr RiverNode -> IO ()
foreign import capi "river-wm.h river_node_v1_place_above"
  river_node_v1_place_above :: Ptr RiverNode -> Ptr RiverNode -> IO ()
