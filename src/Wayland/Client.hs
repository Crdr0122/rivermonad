{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Wayland.Client where

import Foreign
import Foreign.C
import Types

foreign import capi "wayland-client.h wl_display_connect"
  wlDisplayConnect :: CString -> IO (Ptr WlDisplay)

foreign import capi "wayland-client.h wl_display_dispatch"
  wlDisplayDispatch :: Ptr WlDisplay -> IO CInt

foreign import capi "wayland-client.h wl_display_dispatch_pending"
  wlDisplayDispatchPending :: Ptr WlDisplay -> IO CInt

foreign import capi "wayland-client.h wl_display_flush"
  wlDisplayFlush :: Ptr WlDisplay -> IO ()

foreign import capi "wayland-client.h wl_display_get_fd"
  wlDisplayGetFd :: Ptr WlDisplay -> IO CInt

foreign import capi "wayland-client.h wl_display_roundtrip"
  wlDisplayRoundtrip :: Ptr WlDisplay -> IO CInt

foreign import capi "wayland-client.h wl_display_get_registry"
  wlDisplayGetRegistry :: Ptr WlDisplay -> IO (Ptr WlRegistry)

foreign import capi "wayland-client.h wl_registry_bind"
  wlRegistryBind :: Ptr WlRegistry -> CUInt -> Ptr () -> CUInt -> IO (Ptr WlProxy)

foreign import capi "wayland-client.h wl_proxy_add_listener"
  wlProxyAddListener :: Ptr a -> Ptr () -> Ptr () -> IO CInt

foreign import ccall unsafe "get_registry_listener" getRegistryListener :: Ptr ()
foreign import ccall unsafe "get_compositor" getCompositor :: Ptr ()
foreign import ccall unsafe "get_river" getRiver :: Ptr RiverWMManager
foreign import ccall unsafe "get_xkb_bindings" getXkbBindings :: Ptr RiverXkbBindings
foreign import ccall unsafe "get_layer_shell" getLayerShell :: Ptr RiverLayerShell
foreign import ccall unsafe "get_xkb_config" getXkbConfig :: Ptr RiverXkbConfig
foreign import ccall unsafe "get_libinput_config" getLibinputConfig :: Ptr RiverLibinputConfig
foreign import ccall unsafe "get_input_manager" getInputManager :: Ptr RiverInputManager

foreign import ccall unsafe "get_river_wm_listener" getRiverWmListener :: Ptr ()
foreign import ccall unsafe "get_river_window_listener" getRiverWindowListener :: Ptr ()
foreign import ccall unsafe "get_river_output_listener" getRiverOutputListener :: Ptr ()
foreign import ccall unsafe "get_river_seat_listener" getRiverSeatListener :: Ptr ()
foreign import ccall unsafe "get_river_layer_shell_output_listener" getRiverLayerShellOutputListener :: Ptr ()
foreign import ccall unsafe "get_river_layer_shell_seat_listener" getRiverLayerShellSeatListener :: Ptr ()
foreign import ccall unsafe "get_river_xkb_config_listener" getRiverXkbConfigListener :: Ptr ()
foreign import ccall unsafe "get_river_xkb_keyboard_listener" getRiverXkbKeyboardListener :: Ptr ()
foreign import ccall unsafe "get_river_xkb_keymap_listener" getRiverXkbKeymapListener :: Ptr ()
foreign import ccall unsafe "get_river_libinput_config_listener" getRiverLibinputConfigListener :: Ptr ()
foreign import ccall unsafe "get_river_libinput_device_listener" getRiverLibinputDeviceListener :: Ptr ()
foreign import ccall unsafe "get_river_input_manager_listener" getRiverInputManagerListener :: Ptr ()
foreign import ccall unsafe "get_river_input_device_listener" getRiverInputDeviceListener :: Ptr ()
