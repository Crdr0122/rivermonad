module Handlers.InputManagement (mkInputManagerHandlers) where

import Config
import Control.Concurrent.MVar
import Control.Monad (forM_)
import Optics.Core
import Protocols.Generated
import Types
import Wayland.Connection

mkInputManagerHandlers :: MVar WMState -> RiverInputManagerV1Handlers
mkInputManagerHandlers _ =
  RiverInputManagerV1Handlers
    { onRiverInputManagerV1Finished = \m -> riverInputManagerV1Destroy m
    , onRiverInputManagerV1InputDevice = \_ _ -> pure $ Just deviceHandler
    }

deviceHandler :: RiverInputDeviceV1Handlers
deviceHandler =
  RiverInputDeviceV1Handlers
    { onRiverInputDeviceV1Removed = \d -> riverInputDeviceV1Destroy d
    , onRiverInputDeviceV1Type = deviceType
    , onRiverInputDeviceV1Name = \_ _ -> pure ()
    , onRiverInputDeviceV1Done = \_ -> pure ()
    }

deviceType :: Object RiverInputDeviceV1 -> RiverInputDeviceV1TypeEnum -> W ()
deviceType device t = do
  case t of
    RiverInputDeviceV1TypeKeyboard -> forM_ (myConfig ^. #keyboardRepeatInfo) $ \(rate, delay) -> riverInputDeviceV1SetRepeatInfo device rate delay
    _ -> pure ()
