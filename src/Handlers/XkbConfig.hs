module Handlers.XkbConfig (hsXkbConfigFinished, hsXkbConfigXkbKeyboard, hsXkbKeymapSuccess, hsXkbKeymapFailure) where

import Control.Concurrent.MVar
import Foreign
import Foreign.C
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
hsXkbConfigXkbKeyboard dataPtr config keyboard = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state@WMState{currentKeymapFd} -> do
    _ <- wlProxyAddListener (castPtr keyboard) getRiverXkbKeyboardListener dataPtr
    keymap <- riverXkbConfigCreateKeymap config currentKeymapFd 1
    _ <- wlProxyAddListener (castPtr keymap) getRiverXkbKeymapListener (castPtr keyboard)
    -- riverXkbKeyboardNumlockDisable keyboard
    -- riverXkbKeyboardCapslockEnable keyboard
    pure state

foreign export ccall "hs_xkb_keymap_success"
  hsXkbKeymapSuccess :: Ptr () -> Ptr RiverXkbKeymap -> IO ()
foreign export ccall "hs_xkb_keymap_failure"
  hsXkbKeymapFailure :: Ptr () -> Ptr RiverXkbKeymap -> CString -> IO ()

hsXkbKeymapSuccess :: Ptr () -> Ptr RiverXkbKeymap -> IO ()
hsXkbKeymapSuccess keyboard keymap = do
  riverXkbKeyboardSetKeymap (castPtr keyboard) keymap
  riverXkbKeyboardNumlockEnable (castPtr keyboard)

hsXkbKeymapFailure :: Ptr () -> Ptr RiverXkbKeymap -> CString -> IO ()
hsXkbKeymapFailure _ _ errorMsg = do
  e <- peekCString errorMsg
  print $ "Failed creating keymap" ++ e
