module Handlers.Registry (mkRegistryHandlers) where

import Control.Concurrent.MVar
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Data.Map qualified as M
import Data.Set as S (empty)
import Data.Text hiding (show)
import Handlers.InputManagement
import Handlers.LibInput
import Handlers.RiverWM
import Handlers.WlSeat
import Handlers.XkbConfig
import Optics.Core
import Protocols.Generated
import Types
import Wayland.Connection
import Wayland.Generated

mkRegistryHandlers :: MVar WMState -> WlRegistryHandlers
mkRegistryHandlers mvar =
  WlRegistryHandlers
    { onWlRegistryGlobal = bindHandlers mvar
    , onWlRegistryGlobalRemove = removeGlobals mvar
    }

bindHandlers :: MVar WMState -> Object WlRegistry -> Word32 -> Text -> Word32 -> W ()
bindHandlers mvar reg name iface version = case iface of
  "wp_cursor_shape_manager_v1" -> do
    cursor <- wlRegistryBind reg name (min 2 version) WpCursorShapeManagerV1Handlers{}
    modifyMVarW_ mvar $ pure . (#currentCursorShapeManager .~ cursor)
    liftIO $ putStrLn $ "Bound Cursor Shape Manager"
  "river_window_manager_v1" -> do
    wmPtr <- wlRegistryBind reg name (min 5 version) (mkRiverWMHandler mvar)
    modifyMVarW_ mvar $ pure . (#currentWM .~ wmPtr)
    liftIO $ putStrLn $ "Bound Window Manager"
  "river_xkb_bindings_v1" -> do
    xkbBindings <- wlRegistryBind reg name (min 3 version) RiverXkbBindingsV1Handlers{}
    modifyMVarW_ mvar $ pure . (#currentXkbBindings .~ xkbBindings)
    liftIO $ putStrLn $ "Bound Xkb Bindings"
  "river_layer_shell_v1" -> do
    layerShell <- wlRegistryBind reg name (min 1 version) RiverLayerShellV1Handlers{}
    modifyMVarW_ mvar $ pure . (#currentLayerShell .~ layerShell)
    liftIO $ putStrLn $ "Bound Layer Shell"
  "river_input_manager_v1" -> do
    _ <- wlRegistryBind reg name (min 2 version) (mkInputManagerHandlers mvar)
    liftIO $ putStrLn $ "Bound Input Manager"
  "river_libinput_config_v1" -> do
    _ <- wlRegistryBind reg name (min 2 version) (mkLibInputHandlers mvar)
    liftIO $ putStrLn $ "Bound Libinput Config"
  "wl_seat" -> do
    seat <- wlRegistryBind reg name (min 9 version) (mkWlSeatHandlers mvar name)
    modifyMVarW_ mvar $ \state -> do
      let wlSeat =
            WlSeatData
              { wlSeatObj = seat
              , wlSeatCapabilities = S.empty
              , wlPointerSerial = 0
              , wlPointer = Nothing
              , wlCursorShapeDevice = Nothing
              }
      pure $ state & #allWlSeats %~ M.insert name wlSeat
    liftIO $ putStrLn $ "Bound wl_seat: " ++ show name
  "river_xkb_config_v1" -> do
    config <- wlRegistryBind reg name (min 2 version) (mkXkbConfigHandler mvar)
    liftIO $ putStrLn $ "Bound Xkb Config"
    withMVarW mvar $ \WMState{currentKeymapFd} -> do
      forM_ currentKeymapFd $ \fd -> void $ riverXkbConfigV1CreateKeymap config fd RiverXkbConfigV1KeymapFormatTextV1 (mkKeymapHandler mvar)
  _ -> pure ()

removeGlobals :: MVar WMState -> Object WlRegistry -> Word32 -> W ()
removeGlobals mvar _ name = do
  modifyMVarW_ mvar $ \state -> do
    case M.lookup name (state ^. #allWlSeats) of -- Seat Removed
      Nothing -> pure state
      Just wlSeat -> do
        forM_ (wlSeat ^. #wlCursorShapeDevice) wpCursorShapeDeviceV1Destroy
        forM_ (wlSeat ^. #wlPointer) wlPointerRelease
        wlSeatRelease (wlSeat ^. #wlSeatObj)
        pure $ state & #allWlSeats %~ M.delete name
