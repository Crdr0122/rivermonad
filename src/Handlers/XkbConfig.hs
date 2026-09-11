module Handlers.XkbConfig (mkXkbConfigHandler, mkKeymapHandler) where

import Control.Concurrent.MVar
import Control.Monad (forM_, void)
import Control.Monad.IO.Class (liftIO)
import Optics.Core
import Optics.Operators
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
configKbd mvar _ kbd = do
  modifyMVarW_ mvar $ \state@WMState{currentKeyMap} -> do
    case currentKeyMap of
      Left xs -> do
        riverXkbKeyboardV1NumlockEnable kbd
        pure $ state & #currentKeyMap .~ (Left (kbd : xs))
      Right keymap -> do
        riverXkbKeyboardV1SetKeymap kbd keymap >> riverXkbKeyboardV1NumlockEnable kbd
        pure $ state
  pure $ Just kbdHandler

kbdHandler :: RiverXkbKeyboardV1Handlers
kbdHandler =
  RiverXkbKeyboardV1Handlers
    { onRiverXkbKeyboardV1Removed = \kbd -> riverXkbKeyboardV1Destroy kbd
    , onRiverXkbKeyboardV1InputDevice = \_ _ -> pure ()
    , onRiverXkbKeyboardV1Layout = \_ _ _ -> pure ()
    , onRiverXkbKeyboardV1CapslockEnabled = \_ -> pure ()
    , onRiverXkbKeyboardV1CapslockDisabled = \_ -> pure ()
    , onRiverXkbKeyboardV1NumlockEnabled = \_ -> pure ()
    , onRiverXkbKeyboardV1NumlockDisabled = \_ -> pure ()
    , onRiverXkbKeyboardV1Done = \_ -> pure ()
    }

mkKeymapHandler :: MVar WMState -> RiverXkbKeymapV1Handlers
mkKeymapHandler mvar =
  RiverXkbKeymapV1Handlers
    { onRiverXkbKeymapV1Success = keymapSuccess mvar
    , onRiverXkbKeymapV1Failure = \_ e -> liftIO $ print e
    }

keymapSuccess :: MVar WMState -> Object RiverXkbKeymapV1 -> W ()
keymapSuccess mvar keymap = do
  modifyMVarW_ mvar $ \s@WMState{currentKeyMap} -> do
    case currentKeyMap of
      Left xs -> forM_ xs (\kbd -> riverXkbKeyboardV1SetKeymap kbd keymap >> riverXkbKeyboardV1NumlockEnable kbd)
      Right _ -> pure ()
    pure $ s & #currentKeyMap .~ (Right keymap)
