module Handlers.WlSeat where

import Control.Concurrent.MVar
import Control.Monad.State
import Foreign
import Foreign.C
import Optics.Core
import Optics.State
import Optics.State.Operators
import Types
import Wayland.Client
import Wayland.ImportedFunctions

wlSeatCapabilitiesPointer :: CUInt
wlSeatCapabilitiesPointer = 1
wlSeatCapabilitiesKeyboard :: CUInt
wlSeatCapabilitiesKeyboard = 2
wlSeatCapabilitiesTouch :: CUInt
wlSeatCapabilitiesTouch = 4

foreign export ccall "hs_wl_seat_capabilities"
  hsWlSeatCapabilities :: Ptr () -> Ptr WlSeat -> CUInt -> IO ()

foreign export ccall "hs_wl_seat_name"
  hsWlSeatName :: Ptr () -> Ptr WlSeat -> CString -> IO ()

hsWlSeatCapabilities :: Ptr () -> Ptr WlSeat -> CUInt -> IO ()
hsWlSeatCapabilities dataPtr wlSeat capabilities = do
  (stateMVar, name) <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(s :: WMState) ->
    execStateT (transform name) s
 where
  transform name = do
    #allWlSeats % at name %? #wlSeatCapabilities .= capabilities
    let hasPointer = capabilities .&. wlSeatCapabilitiesPointer /= 0
    if hasPointer
      then do
        pointerPtr <- liftIO $ wlSeatGetPointer wlSeat
        _ <- liftIO $ wlProxyAddListener (castPtr pointerPtr) getWlPointerListener dataPtr

        cursorManager <- use #currentCursorShapeManager
        device <- liftIO $ cursorShapeManagerGetPointer cursorManager pointerPtr
        #allWlSeats % at name %? #wlCursorShapeDevice ?= device
        #allWlSeats % at name %? #wlPointer ?= pointerPtr
      else do
        #allWlSeats % at name %? #wlPointer .= Nothing
        #allWlSeats % at name %? #wlCursorShapeDevice .= Nothing

        preuse (#allWlSeats % at name %? #wlCursorShapeDevice % _Just) >>= \case
          Nothing -> pure ()
          Just pointerPtr -> liftIO $ cursorShapeDeviceDestroy pointerPtr
        preuse (#allWlSeats % at name %? #wlPointer % _Just) >>= \case
          Nothing -> pure ()
          Just pointerPtr -> liftIO $ wlPointerRelease pointerPtr

hsWlSeatName :: Ptr () -> Ptr WlSeat -> CString -> IO ()
hsWlSeatName _ _ _ = pure ()

foreign export ccall "hs_wl_pointer_enter"
  hsWlPointerEnter :: Ptr () -> Ptr WlPointer -> CUInt -> Ptr () -> WlFixedT -> WlFixedT -> IO ()

hsWlPointerEnter :: Ptr () -> Ptr WlPointer -> CUInt -> Ptr () -> WlFixedT -> WlFixedT -> IO ()
hsWlPointerEnter dataPtr _ serial _ _ _ = do
  (stateMVar, name) <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(s :: WMState) -> do
    pure $ s & #allWlSeats % at name %? #wlPointerSerial .~ serial
