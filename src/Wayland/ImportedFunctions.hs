{-# LANGUAGE CApiFFI #-}

module Wayland.ImportedFunctions where

import Foreign
import Foreign.C
import Types

foreign import ccall unsafe "get_river" getRiver :: Ptr RiverWMManager
foreign import ccall unsafe "get_river_wm_listener" getRiverWmListener :: Ptr ()
foreign import ccall unsafe "get_river_window_listener" getRiverWindowListener :: Ptr ()
foreign import ccall unsafe "get_river_output_listener" getRiverOutputListener :: Ptr ()
foreign import ccall unsafe "get_river_seat_listener" getRiverSeatListener :: Ptr ()
foreign import ccall unsafe "get_river_layer_shell_output_listener" getRiverLayerShellOutputListener :: Ptr ()
foreign import ccall unsafe "get_river_layer_shell_seat_listener" getRiverLayerShellSeatListener :: Ptr ()
foreign import ccall unsafe "get_river_xkb_config_listener" getRiverXkbConfigListener :: Ptr ()
foreign import ccall unsafe "get_river_xkb_keyboard_listener" getRiverXkbKeyboardListener :: Ptr ()

foreign import capi "river-window-management.h river_window_manager_v1_stop"
  riverWindowManagerStop :: Ptr RiverWMManager -> IO ()
foreign import capi "river-window-management.h river_window_manager_v1_destroy"
  riverWindowManagerDestroy :: Ptr RiverWMManager -> IO ()
foreign import capi "river-window-management.h river_window_manager_v1_manage_finish"
  riverWindowManagerManageFinish :: Ptr RiverWMManager -> IO ()
foreign import capi "river-window-management.h river_window_manager_v1_manage_dirty"
  riverWindowManagerManageDirty :: Ptr RiverWMManager -> IO ()
foreign import capi "river-window-management.h river_window_manager_v1_render_finish"
  riverWindowManagerRenderFinish :: Ptr RiverWMManager -> IO ()
foreign import capi "river-window-management.h river_window_manager_v1_get_shell_surface"
  riverWindowManagerGetShellSurface :: Ptr RiverWMManager -> Ptr WlSurface -> IO ()
foreign import capi "river-window-management.h river_window_manager_v1_exit_session"
  riverWindowManagerExitSession :: Ptr RiverWMManager -> IO ()

foreign import capi "river-window-management.h river_window_v1_destroy"
  riverWindowDestroy :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_close"
  riverWindowClose :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_get_node"
  riverWindowGetNode :: Ptr RiverWindow -> IO (Ptr RiverNode)
foreign import capi "river-window-management.h river_window_v1_propose_dimensions"
  riverWindowProposeDimensions :: Ptr RiverWindow -> CInt -> CInt -> IO ()
foreign import capi "river-window-management.h river_window_v1_hide"
  riverWindowHide :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_show"
  riverWindowShow :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_use_csd"
  riverWindowUseCsd :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_use_ssd"
  riverWindowUseSsd :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_set_borders"
  riverWindowSetBorders :: Ptr RiverWindow -> RiverEdge -> CInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO ()
foreign import capi "river-window-management.h river_window_v1_set_tiled"
  riverWindowSetTiled :: Ptr RiverWindow -> RiverEdge -> IO ()
foreign import capi "river-window-management.h river_window_v1_get_decoration_above"
  riverWindowGetDecorationAbove :: Ptr RiverWindow -> Ptr WlSurface -> IO (Ptr ())
foreign import capi "river-window-management.h river_window_v1_get_decoration_below"
  riverWindowGetDecorationBelow :: Ptr RiverWindow -> Ptr WlSurface -> IO (Ptr ())
foreign import capi "river-window-management.h river_window_v1_inform_resize_start"
  riverWindowInformResizeStart :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_inform_resize_end"
  riverWindowInformResizeEnd :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_set_capabilities"
  riverWindowSetCapabilities :: Ptr RiverWindow -> CUInt -> IO ()
foreign import capi "river-window-management.h river_window_v1_inform_maximized"
  riverWindowInformMaximized :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_inform_unmaximized"
  riverWindowInformUnmaximized :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_inform_fullscreen"
  riverWindowInformFullscreen :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_inform_not_fullscreen"
  riverWindowInformNotFullscreen :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_fullscreen"
  riverWindowFullscreen :: Ptr RiverWindow -> Ptr RiverOutput -> IO ()
foreign import capi "river-window-management.h river_window_v1_exit_fullscreen"
  riverWindowExitFullscreen :: Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_window_v1_set_clip_box"
  riverWindowSetClipBox :: Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import capi "river-window-management.h river_window_v1_set_content_clip_box"
  riverWindowSetContentClipBox :: Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign import capi "river-window-management.h river_window_v1_set_dimension_bounds"
  riverWindowSetDimensionBounds :: Ptr RiverWindow -> CInt -> CInt -> IO ()

foreign import capi "river-window-management.h river_node_v1_destroy"
  riverNodeDestroy :: Ptr RiverNode -> IO ()
foreign import capi "river-window-management.h river_node_v1_set_position"
  riverNodeSetPosition :: Ptr RiverNode -> CInt -> CInt -> IO ()
foreign import capi "river-window-management.h river_node_v1_place_top"
  riverNodePlaceTop :: Ptr RiverNode -> IO ()
foreign import capi "river-window-management.h river_node_v1_place_bottom"
  riverNodePlaceBottom :: Ptr RiverNode -> IO ()
foreign import capi "river-window-management.h river_node_v1_place_above"
  riverNodePlaceAbove :: Ptr RiverNode -> Ptr RiverNode -> IO ()
foreign import capi "river-window-management.h river_node_v1_place_below"
  riverNodePlaceBelow :: Ptr RiverNode -> Ptr RiverNode -> IO ()

foreign import capi "river-window-management.h river_output_v1_destroy"
  riverOutputDestroy :: Ptr RiverOutput -> IO ()
foreign import capi "river-window-management.h river_output_v1_set_presentation_mode"
  riverOutputSetPresentationMode :: Ptr RiverOutput -> CUInt -> IO ()

foreign import capi "river-window-management.h river_seat_v1_destroy"
  riverSeatDestroy :: Ptr RiverSeat -> IO ()
foreign import capi "river-window-management.h river_seat_v1_focus_window"
  riverSeatFocusWindow :: Ptr RiverSeat -> Ptr RiverWindow -> IO ()
foreign import capi "river-window-management.h river_seat_v1_focus_shell_surface"
  riverSeatFocusShellSurface :: Ptr RiverSeat -> Ptr RiverShellSurface -> IO ()
foreign import capi "river-window-management.h river_seat_v1_clear_focus"
  riverSeatClearFocus :: Ptr RiverSeat -> IO ()
foreign import capi "river-window-management.h river_seat_v1_op_start_pointer"
  riverSeatOpStartPointer :: Ptr RiverSeat -> IO ()
foreign import capi "river-window-management.h river_seat_v1_op_end"
  riverSeatOpEnd :: Ptr RiverSeat -> IO ()
foreign import capi "river-window-management.h river_seat_v1_get_pointer_binding"
  riverSeatGetPointerBinding :: Ptr RiverSeat -> CUInt -> CUInt -> IO (Ptr RiverPointerBinding)
foreign import capi "river-window-management.h river_seat_v1_set_xcursor_theme"
  riverSeatSetXcursorTheme :: Ptr RiverSeat -> CString -> CUInt -> IO ()
foreign import capi "river-window-management.h river_seat_v1_pointer_warp"
  riverSeatPointerWarp :: Ptr RiverSeat -> CInt -> CInt -> IO ()

foreign import capi "river-window-management.h river_pointer_binding_v1_destroy"
  riverPointerBindingDestroy :: Ptr RiverPointerBinding -> IO ()
foreign import capi "river-window-management.h river_pointer_binding_v1_enable"
  riverPointerBindingEnable :: Ptr RiverPointerBinding -> IO ()
foreign import capi "river-window-management.h river_pointer_binding_v1_disable"
  riverPointerBindingDisable :: Ptr RiverPointerBinding -> IO ()

foreign import capi "river-xkb-bindings.h river_xkb_bindings_v1_get_xkb_binding"
  riverXkbBindingsGetXkbBinding :: Ptr RiverXkbBindings -> Ptr RiverSeat -> CUInt -> CUInt -> IO (Ptr RiverXkbBinding)
foreign import capi "river-xkb-bindings.h river_xkb_bindings_v1_destroy"
  riverXkbBindingsDestroy :: Ptr RiverXkbBindings -> IO ()
foreign import capi "river-xkb-bindings.h river_xkb_binding_v1_enable"
  riverXkbBindingEnable :: Ptr RiverXkbBinding -> IO ()

foreign import capi "river-layer-shell.h river_layer_shell_v1_destroy"
  riverLayerShellDestroy :: Ptr RiverLayerShell -> IO ()
foreign import capi "river-layer-shell.h river_layer_shell_v1_get_output"
  riverLayerShellGetOutput :: Ptr RiverLayerShell -> Ptr RiverOutput -> IO (Ptr RiverLayerShellOutput)
foreign import capi "river-layer-shell.h river_layer_shell_v1_get_seat"
  riverLayerShellGetSeat :: Ptr RiverLayerShell -> Ptr RiverSeat -> IO (Ptr RiverLayerShellSeat)

foreign import capi "river-layer-shell.h river_layer_shell_output_v1_destroy"
  riverLayerShellOutputDestroy :: Ptr RiverLayerShellOutput -> IO ()
foreign import capi "river-layer-shell.h river_layer_shell_output_v1_set_default"
  riverLayerShellOutputSetDefault :: Ptr RiverLayerShellOutput -> IO ()

foreign import capi "river-layer-shell.h river_layer_shell_seat_v1_destroy"
  riverLayerShellSeatDestroy :: Ptr RiverLayerShellSeat -> IO ()

foreign import capi "river-xkb-config.h river_xkb_config_v1_stop"
  riverXkbConfigStop :: Ptr RiverXkbConfig -> IO ()
foreign import capi "river-xkb-config.h river_xkb_config_v1_destroy"
  riverXkbConfigDestroy :: Ptr RiverXkbConfig -> IO ()
foreign import capi "river-xkb-config.h river_xkb_config_v1_create_keymap"
  riverXkbConfigCreateKeymap :: Ptr RiverXkbConfig -> CInt -> CUInt -> IO (Ptr RiverXkbKeymap)

foreign import capi "river-xkb-config.h river_xkb_keyboard_v1_destroy"
  riverXkbKeyboardDestroy :: Ptr RiverXkbKeyboard -> IO ()
foreign import capi "river-xkb-config.h river_xkb_keyboard_v1_set_keymap"
  riverXkbKeyboardSetKeymap :: Ptr RiverXkbKeyboard -> Ptr RiverXkbKeymap -> IO ()
foreign import capi "river-xkb-config.h river_xkb_keyboard_v1_set_layout_by_index"
  riverXkbKeyboardSetLayoutByIndex :: Ptr RiverXkbKeyboard -> CInt -> IO ()
foreign import capi "river-xkb-config.h river_xkb_keyboard_v1_set_layout_by_name"
  riverXkbKeyboardSetLayoutByName :: Ptr RiverXkbKeyboard -> CString -> IO ()
foreign import capi "river-xkb-config.h river_xkb_keyboard_v1_capslock_enable"
  riverXkbKeyboardCapslockEnable :: Ptr RiverXkbKeyboard -> IO ()
foreign import capi "river-xkb-config.h river_xkb_keyboard_v1_capslock_disable"
  riverXkbKeyboardCapslockDisable :: Ptr RiverXkbKeyboard -> IO ()
foreign import capi "river-xkb-config.h river_xkb_keyboard_v1_numlock_enable"
  riverXkbKeyboardNumlockEnable :: Ptr RiverXkbKeyboard -> IO ()
foreign import capi "river-xkb-config.h river_xkb_keyboard_v1_numlock_disable"
  riverXkbKeyboardNumlockDisable :: Ptr RiverXkbKeyboard -> IO ()
