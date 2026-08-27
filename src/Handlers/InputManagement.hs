module Handlers.InputManagement where

import Config
import Control.Concurrent.MVar
import Control.Monad (forM_, void)
import Foreign hiding (void)
import Foreign.C
import Optics.Core
import Types
import Wayland.Client
import Wayland.ImportedFunctions

foreign export ccall "hs_input_manager_input_device"
  hsInputManagerInputDevice :: Ptr () -> Ptr RiverInputManager -> Ptr RiverInputDevice -> IO ()
foreign export ccall "hs_input_manager_finished"
  hsInputManagerFinished :: Ptr () -> Ptr RiverInputManager -> IO ()

hsInputManagerFinished :: Ptr () -> Ptr RiverInputManager -> IO ()
hsInputManagerFinished _ manager = riverInputManagerDestroy manager

hsInputManagerInputDevice :: Ptr () -> Ptr RiverInputManager -> Ptr RiverInputDevice -> IO ()
hsInputManagerInputDevice dataPtr _ device = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> do
    void $ wlProxyAddListener (castPtr device) getRiverInputDeviceListener dataPtr
    pure state

foreign export ccall "hs_input_device_name"
  hsInputDeviceName :: Ptr () -> Ptr RiverInputDevice -> CString -> IO ()
foreign export ccall "hs_input_device_type"
  hsInputDeviceType :: Ptr () -> Ptr RiverInputDevice -> CUInt -> IO ()
foreign export ccall "hs_input_device_done"
  hsInputDeviceDone :: Ptr () -> Ptr RiverInputDevice -> IO ()
foreign export ccall "hs_input_device_removed"
  hsInputDeviceRemoved :: Ptr () -> Ptr RiverInputDevice -> IO ()

hsInputDeviceRemoved :: Ptr () -> Ptr RiverInputDevice -> IO ()
hsInputDeviceRemoved _ device = riverInputDeviceDestroy device

hsInputDeviceName :: Ptr () -> Ptr RiverInputDevice -> CString -> IO ()
hsInputDeviceName _ _ _ = pure ()

hsInputDeviceType :: Ptr () -> Ptr RiverInputDevice -> CUInt -> IO ()
hsInputDeviceType dataPtr device t = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> do
    case t of
      -- Keyboard
      0 -> forM_ (myConfig ^. #keyboardRepeatInfo) $ \(rate, delay) -> riverInputDeviceSetRepeatInfo device rate delay
      -- Pointer
      1 -> pure ()
      -- Touch
      2 -> pure ()
      -- Tablet
      3 -> pure ()
      -- Error
      _ -> pure ()
    pure state

hsInputDeviceDone :: Ptr () -> Ptr RiverInputDevice -> IO ()
hsInputDeviceDone _ _ = pure ()
