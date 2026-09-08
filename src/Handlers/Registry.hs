module Handlers.Registry where

import Control.Concurrent.MVar
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Data.Text hiding (show)
import Foreign
import Foreign.C
import Optics.Core
import Protocols.Generated
import Types
import Wayland.Connection
import Wayland.Generated

mkRegistryHandlers :: MVar WMState -> WlRegistryHandlers
mkRegistryHandlers mvar =
  WlRegistryHandlers
    { onWlRegistryGlobal = bindCompositor
    , onWlRegistryGlobalRemove = removeGlobals
    }

bindCompositor :: Object WlRegistry -> Word32 -> Text -> Word32 -> W ()
bindCompositor obj name iface version = case iface of
  "wl_compositor" -> do
    compositor <- wlRegistryBind obj name version WlCompositorHandlers{}
    liftIO $ putStrLn ("bound wl_compositor as " <> show compositor)
  _ -> pure ()

removeGlobals :: Object WlRegistry -> Word32 -> W ()
removeGlobals _ _ = pure ()

-- registryGlobal :: Ptr () -> Ptr WlRegistry -> CUInt -> CString -> CUInt -> IO ()
-- registryGlobal dataPtr registry name interfacePtr version = do
--   (stateMVar :: MVar WMState) <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   interface <- peekCString interfacePtr
--   case interface of
--     "wl_compositor" -> pure ()
--     "wp_cursor_shape_manager_v1" -> do
--       cursor <- wlRegistryBind registry name cursor_shape_manager_v1_interface (min 2 version)
--       modifyMVar_ stateMVar $ pure . (#currentCursorShapeManager .~ (castPtr cursor))
--     "wl_seat" -> do
--       seatPtr <- wlRegistryBind registry name wl_seat_interface (min 9 version)
--       modifyMVar_ stateMVar $ \state -> do
--         doublePtr <- newStablePtr (stateMVar, name)
--         let wlSeat =
--               WlSeatData
--                 { wlSeatPtr = (castPtr seatPtr)
--                 , wlSeatListenerHsPtr = Just doublePtr
--                 , wlSeatCapabilities = 0
--                 , wlPointerSerial = 0
--                 , wlPointer = Nothing
--                 , wlCursorShapeDevice = Nothing
--                 }
--         _ <- wlProxyAddListener (castPtr seatPtr) getWlSeatListener (castStablePtrToPtr doublePtr)
--         pure $ (state & #allWlSeats %~ M.insert name wlSeat)
--       putStrLn $ "Bound wl_seat: " ++ show name
--     "river_window_manager_v1" -> do
--       wmPtr <- wlRegistryBind registry name river_window_manager_v1_interface (min 5 version)
--       _ <- wlProxyAddListener (castPtr wmPtr) getRiverWmListener dataPtr
--       modifyMVar_ stateMVar $ pure . (#currentWindowManager .~ (castPtr wmPtr))
--       putStrLn $ "Bound Window Manager"
--     "river_xkb_bindings_v1" -> do
--       xkbBindings <- wlRegistryBind registry name river_xkb_bindings_v1_interface (min 3 version)
--       modifyMVar_ stateMVar $ pure . (#currentXkbBindings .~ (castPtr xkbBindings))
--       putStrLn $ "Bound Xkb Bindings"
--     "river_layer_shell_v1" -> do
--       layerShell <- wlRegistryBind registry name river_layer_shell_v1_interface (min 1 version)
--       modifyMVar_ stateMVar $ pure . (#currentLayerShell .~ (castPtr layerShell))
--       putStrLn $ "Bound Layer Shell"
--     "river_input_manager_v1" -> do
--       inputManager <- wlRegistryBind registry name river_input_manager_v1_interface (min 2 version)
--       _ <- wlProxyAddListener (castPtr inputManager) getRiverInputManagerListener dataPtr
--       putStrLn $ "Bound Input Manager"
--     "river_libinput_config_v1" -> do
--       libinput <- wlRegistryBind registry name river_libinput_config_v1_interface (min 2 version)
--       _ <- wlProxyAddListener (castPtr libinput) getRiverLibinputConfigListener dataPtr
--       putStrLn $ "Bound Libinput Config"
--     "river_xkb_config_v1" -> do
--       xkbConfig <- wlRegistryBind registry name river_xkb_config_v1_interface (min 2 version)
--       _ <- wlProxyAddListener (castPtr xkbConfig) getRiverXkbConfigListener dataPtr
--       putStrLn $ "Bound Xkb Config"
--     _ -> pure ()
--
-- registryGlobalRemove :: Ptr () -> Ptr WlRegistry -> CUInt -> IO ()
-- registryGlobalRemove dataPtr _ name = do
--   (stateMVar :: MVar WMState) <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ stateMVar $ \state -> do
--     case M.lookup name (state ^. #allWlSeats) of -- Seat Removed
--       Nothing -> pure state
--       Just wlSeat -> do
--         forM_ (wlSeat ^. #wlCursorShapeDevice) cursorShapeDeviceDestroy
--         forM_ (wlSeat ^. #wlPointer) wlPointerRelease
--         forM_ (wlSeat ^. #wlSeatListenerHsPtr) freeStablePtr
--
--         wlSeatRelease (wlSeat ^. #wlSeatPtr)
--
--         pure $ state & #allWlSeats %~ M.delete name
