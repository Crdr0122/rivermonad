module Handlers.XkbConfig (mkXkbConfigHandler) where

import Control.Concurrent.MVar
import Control.Monad (void, forM_)
import Control.Monad.IO.Class (liftIO)
import Protocols.Generated
import Types
import Wayland.Connection

mkXkbConfigHandler :: MVar WMState -> RiverXkbConfigV1Handlers
mkXkbConfigHandler mvar =
  RiverXkbConfigV1Handlers
    { onRiverXkbConfigV1Finished = \config -> riverXkbConfigV1Destroy config
    , onRiverXkbConfigV1XkbKeyboard = configKbd mvar
    }

configKbd :: MVar WMState -> Object RiverXkbConfigV1 -> Object RiverXkbKeyboardV1 -> W (Maybe RiverXkbKeyboardV1Handlers)
configKbd mvar config kbd = do
  modifyMVarW_ mvar $ \state@WMState{currentKeymapFd} -> do
    forM_ currentKeymapFd $ \fd ->
        void $ riverXkbConfigV1CreateKeymap config fd RiverXkbConfigV1KeymapFormatTextV1 (keymapHandler kbd)
    riverXkbKeyboardV1NumlockEnable kbd
    pure state
  pure $ Just kbdHandler

kbdHandler :: RiverXkbKeyboardV1Handlers
kbdHandler =
  RiverXkbKeyboardV1Handlers
    { onRiverXkbKeyboardV1Removed = \kbd -> riverXkbKeyboardV1Destroy kbd
    , onRiverXkbKeyboardV1InputDevice = \kbd _ -> riverXkbKeyboardV1NumlockEnable kbd
    , onRiverXkbKeyboardV1Layout = \_ _ _ -> pure ()
    , onRiverXkbKeyboardV1CapslockEnabled = \_ -> pure ()
    , onRiverXkbKeyboardV1CapslockDisabled = \_ -> pure ()
    , onRiverXkbKeyboardV1NumlockEnabled = \_ -> pure ()
    , onRiverXkbKeyboardV1NumlockDisabled = \_ -> pure ()
    , onRiverXkbKeyboardV1Done = \_ -> pure ()
    }

keymapHandler :: Object RiverXkbKeyboardV1 -> RiverXkbKeymapV1Handlers
keymapHandler kbd =
  RiverXkbKeymapV1Handlers
    { onRiverXkbKeymapV1Success = \keymap -> riverXkbKeyboardV1SetKeymap kbd keymap >> riverXkbKeyboardV1NumlockEnable kbd
    , onRiverXkbKeymapV1Failure = \_ e -> liftIO $ print e
    }
