{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Wayland.Client where

import Foreign
import Foreign.C
import Types

foreign import capi "wayland-client.h wl_display_connect"
  wl_display_connect :: CString -> IO (Ptr WlDisplay)

foreign import capi "wayland-client.h wl_display_dispatch"
  wl_display_dispatch :: Ptr WlDisplay -> IO CInt

foreign import capi "wayland-client.h wl_display_flush"
  wl_display_flush :: Ptr WlDisplay -> IO ()

foreign import capi "wayland-client.h wl_display_get_fd"
  wl_display_get_fd :: Ptr WlDisplay -> IO (CInt)

foreign import capi "wayland-client.h wl_display_roundtrip"
  wl_display_roundtrip :: Ptr WlDisplay -> IO CInt

foreign import capi "wayland-client.h wl_display_get_registry"
  wl_display_get_registry :: Ptr WlDisplay -> IO (Ptr WlRegistry)

foreign import capi "wayland-client.h wl_registry_bind"
  wl_registry_bind ::
    Ptr WlRegistry ->
    Word32 ->
    Ptr () ->
    Word32 ->
    IO (Ptr WlProxy)

foreign import capi "wayland-client.h wl_proxy_add_listener"
  wl_proxy_add_listener ::
    Ptr a ->
    Ptr () ->
    Ptr () ->
    IO CInt

foreign import ccall unsafe "get_registry_listener" get_registry_listener :: Ptr ()
foreign import ccall unsafe "get_compositor" get_compositor :: Ptr ()
