module Handlers.PointerBindings where

import Control.Concurrent.MVar
import Foreign
import Optics.Core
import Types
import Utils.Keysyms
import Wayland.Client
import Wayland.ImportedFunctions

data PointerBindingListener = PointerBindingListener
  { pointerPressed :: FunPtr PointerCallback
  , pointerReleased :: FunPtr PointerCallback
  }

instance Storable PointerBindingListener where
  sizeOf _ = sizeOf (nullPtr :: Ptr ()) * 2
  alignment _ = alignment (nullPtr :: Ptr ())
  poke ptr (PointerBindingListener p r) = do
    let pSize = sizeOf (nullPtr :: Ptr ())
    pokeByteOff ptr (pSize * 0) p
    pokeByteOff ptr (pSize * 1) r
  peek ptr = do
    let offset = sizeOf (nullPtr :: Ptr ())
    pressed <- peek (castPtr ptr) :: IO (FunPtr PointerCallback)
    released <- peekByteOff ptr offset :: IO (FunPtr PointerCallback)
    pure $ PointerBindingListener pressed released

foreign import ccall "wrapper"
  mkPointerCallback :: PointerCallback -> IO (FunPtr PointerCallback)

registerPointerbind :: Ptr () -> Ptr RiverSeat -> (PointerBtn, KeyMod) -> (Ptr RiverSeat -> MVar WMState -> IO (), Ptr RiverSeat -> MVar WMState -> IO ()) -> IO ()
registerPointerbind dataPtr seat (PointerBtn key, KeyMod modifier) (onPressed, onReleased) = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> do
    pressedPtr <- mkPointerCallback (\d _ -> deRefStablePtr (castPtrToStablePtr d) >>= onPressed seat)
    releasedPtr <- mkPointerCallback (\d _ -> deRefStablePtr (castPtrToStablePtr d) >>= onReleased seat)

    let listener = PointerBindingListener pressedPtr releasedPtr
    listenerPtr <- malloc :: IO (Ptr PointerBindingListener)
    poke listenerPtr listener
    newBinding <- riverSeatGetPointerBinding seat key modifier
    _ <- wlProxyAddListener (castPtr newBinding) (castPtr listenerPtr) dataPtr

    pure $
      state
        & (#manageQueue <>~ riverPointerBindingEnable newBinding)
        & (#allSeats % at seat %? #pointerBindings %~ (newBinding :))
