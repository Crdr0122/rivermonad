module Handlers.XkbConfig where

import Data.IORef
import Foreign
import Types
import Wayland.Client
import Wayland.ImportedFunctions

foreign export ccall "hs_xkb_config_finished"
  hsXkbConfigFinished :: Ptr () -> Ptr RiverXkbConfig -> IO ()
foreign export ccall "hs_xkb_config_xkb_keyboard"
  hsXkbConfigXkbKeyboard :: Ptr () -> Ptr RiverXkbConfig -> Ptr RiverXkbKeyboard -> IO ()

hsXkbConfigFinished :: Ptr () -> Ptr RiverXkbConfig -> IO ()
hsXkbConfigFinished _ config = riverXkbConfigDestroy config

hsXkbConfigXkbKeyboard :: Ptr () -> Ptr RiverXkbConfig -> Ptr RiverXkbKeyboard -> IO ()
hsXkbConfigXkbKeyboard dataPtr _ keyboard = do
  _ <- wlProxyAddListener (castPtr keyboard) getRiverXkbKeyboardListener dataPtr
  riverXkbKeyboardNumlockEnable keyboard
