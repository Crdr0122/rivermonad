module Handlers.Registry where

import Control.Concurrent.MVar
import Data.Bimap qualified as B
import Foreign
import Foreign.C
import Optics.Core
import Types
import Wayland.Client

type RegistryGlobalCallback = Ptr () -> Ptr WlRegistry -> CUInt -> CString -> CUInt -> IO ()
type RegistryGlobalRemoveCallback = Ptr () -> Ptr WlRegistry -> CUInt -> IO ()

foreign import ccall "wrapper"
  makeRegistryGlobalCallback :: RegistryGlobalCallback -> IO (FunPtr RegistryGlobalCallback)
foreign import ccall "wrapper"
  makeRegistryGlobalRemoveCallback :: RegistryGlobalRemoveCallback -> IO (FunPtr RegistryGlobalRemoveCallback)

data WlRegistryListener = WlRegistryListener
  { wlRegistryGlobal :: FunPtr RegistryGlobalCallback
  , wlRegistryGlobalRemove :: FunPtr RegistryGlobalRemoveCallback
  }

foreign import ccall "&wl_seat_interface" wl_seat_interface :: Ptr WlInterface
foreign import ccall "&wl_compositor_interface" wl_compositor_interface :: Ptr WlInterface
foreign import ccall "&river_window_manager_v1_interface" river_window_manager_v1_interface :: Ptr WlInterface
foreign import ccall "&river_xkb_bindings_v1_interface" river_xkb_bindings_v1_interface :: Ptr WlInterface
foreign import ccall "&river_layer_shell_v1_interface" river_layer_shell_v1_interface :: Ptr WlInterface
foreign import ccall "&river_input_manager_v1_interface" river_input_manager_v1_interface :: Ptr WlInterface
foreign import ccall "&river_libinput_config_v1_interface" river_libinput_config_v1_interface :: Ptr WlInterface
foreign import ccall "&river_xkb_config_v1_interface" river_xkb_config_v1_interface :: Ptr WlInterface
foreign import ccall "&cursor_shape_manager_v1_interface" cursor_shape_manager_v1_interface :: Ptr WlInterface

instance Storable WlRegistryListener where
  sizeOf _ = sizeOf (nullPtr :: Ptr ()) * 2
  alignment _ = alignment (nullPtr :: Ptr ())
  peek ptr = do
    let offset = sizeOf (nullPtr :: Ptr ())
    reg <- peek (castPtr ptr) :: IO (FunPtr RegistryGlobalCallback)
    regRemove <- peekByteOff ptr offset :: IO (FunPtr RegistryGlobalRemoveCallback)
    pure $ WlRegistryListener reg regRemove
  poke p listener = do
    poke (castPtr p) (wlRegistryGlobal listener)
    poke (castPtr p `plusPtr` sizeOf (nullPtr :: Ptr ())) (wlRegistryGlobalRemove listener)

registryGlobal :: Ptr () -> Ptr WlRegistry -> CUInt -> CString -> CUInt -> IO ()
registryGlobal dataPtr registry name interfacePtr version = do
  (stateMVar :: MVar WMState) <- deRefStablePtr (castPtrToStablePtr dataPtr)
  interface <- peekCString interfacePtr
  case interface of
    "wl_compositor" -> do
      putStrLn $ "Bound Compositor"
    "wp_cursor_shape_manager_v1" -> do
      cursor <- wlRegistryBind registry name cursor_shape_manager_v1_interface (min 2 version)
      modifyMVar_ stateMVar $ pure . (#currentCursorShapeManager .~ (castPtr cursor))
    "wl_seat" -> do
      seatPtr <- wlRegistryBind registry name wl_seat_interface (min 9 version)
      modifyMVar_ stateMVar $ \state -> do
        pure $ (state & #allWlSeats %~ B.insert name (castPtr seatPtr))
      putStrLn $ "Bound wl_seat: " ++ show name
    "river_window_manager_v1" -> do
      wmPtr <- wlRegistryBind registry name river_window_manager_v1_interface (min 4 version)
      _ <- wlProxyAddListener (castPtr wmPtr) getRiverWmListener dataPtr
      modifyMVar_ stateMVar $ pure . (#currentWindowManager .~ (castPtr wmPtr))
      putStrLn $ "Bound Window Manager"
    "river_xkb_bindings_v1" -> do
      xkbBindings <- wlRegistryBind registry name river_xkb_bindings_v1_interface (min 2 version)
      modifyMVar_ stateMVar $ pure . (#currentXkbBindings .~ (castPtr xkbBindings))
      putStrLn $ "Bound Xkb Bindings"
    "river_layer_shell_v1" -> do
      layerShell <- wlRegistryBind registry name river_layer_shell_v1_interface (min 1 version)
      modifyMVar_ stateMVar $ pure . (#currentLayerShell .~ (castPtr layerShell))
      putStrLn $ "Bound Layer Shell"
    "river_input_manager_v1" -> do
      inputManager <- wlRegistryBind registry name river_input_manager_v1_interface (min 1 version)
      _ <- wlProxyAddListener (castPtr inputManager) getRiverInputManagerListener dataPtr
      putStrLn $ "Bound Input Manager"
    "river_libinput_config_v1" -> do
      libinput <- wlRegistryBind registry name river_libinput_config_v1_interface (min 1 version)
      _ <- wlProxyAddListener (castPtr libinput) getRiverLibinputConfigListener dataPtr
      putStrLn $ "Bound Libinput Config"
    "river_xkb_config_v1" -> do
      xkbConfig <- wlRegistryBind registry name river_xkb_config_v1_interface (min 1 version)
      _ <- wlProxyAddListener (castPtr xkbConfig) getRiverXkbConfigListener dataPtr
      putStrLn $ "Bound Xkb Config"
    _ -> pure ()

registryGlobalRemove :: Ptr () -> Ptr WlRegistry -> CUInt -> IO ()
registryGlobalRemove _ _ _ = pure ()
