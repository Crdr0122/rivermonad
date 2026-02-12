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

foreign import capi "wayland-client.h wl_display_flush"
  wlDisplayFlush :: Ptr WlDisplay -> IO ()

foreign import capi "wayland-client.h wl_display_get_fd"
  wlDisplayGetFd :: Ptr WlDisplay -> IO (CInt)

foreign import capi "wayland-client.h wl_display_roundtrip"
  wlDisplayRoundtrip :: Ptr WlDisplay -> IO CInt

foreign import capi "wayland-client.h wl_display_get_registry"
  wlDisplayGetRegistry :: Ptr WlDisplay -> IO (Ptr WlRegistry)

foreign import capi "wayland-client.h wl_registry_bind"
  wlRegistryBind ::
    Ptr WlRegistry ->
    CUInt ->
    Ptr () ->
    CUInt ->
    IO (Ptr WlProxy)

foreign import capi "wayland-client.h wl_proxy_add_listener"
  wlProxyAddListener ::
    Ptr a ->
    Ptr () ->
    Ptr () ->
    IO CInt

foreign import ccall unsafe "get_registry_listener" getRegistryListener :: Ptr ()
foreign import ccall unsafe "get_compositor" getCompositor :: Ptr ()
foreign import ccall unsafe "get_xkb_bindings" getXkbBindings :: Ptr RiverXkbBindings
