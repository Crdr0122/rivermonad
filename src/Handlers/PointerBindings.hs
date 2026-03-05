module Handlers.PointerBindings where

import Control.Concurrent.MVar
import Foreign
import Foreign.C
import Types
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
    poke (ptr `plusPtr` (pSize * 0)) p
    poke (ptr `plusPtr` (pSize * 1)) r
  peek ptr = do
    let offset = sizeOf (nullPtr :: Ptr ())
    pressed <- peek (castPtr ptr) :: IO (FunPtr PointerCallback)
    released <- peekByteOff ptr offset :: IO (FunPtr PointerCallback)
    pure $ PointerBindingListener pressed released

foreign import ccall "wrapper"
  mkPointerCallback :: PointerCallback -> IO (FunPtr PointerCallback)

registerPointerbind :: Ptr () -> Ptr RiverSeat -> (CUInt, CUInt, MVar WMState -> IO (), MVar WMState -> IO ()) -> IO ()
registerPointerbind dataPtr seat (key, modifier, onPressed, onReleased) = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    pressedPtr <- mkPointerCallback (\d _ -> deRefStablePtr (castPtrToStablePtr d) >>= onPressed)
    releasedPtr <- mkPointerCallback (\d _ -> deRefStablePtr (castPtrToStablePtr d) >>= onReleased)

    let listener = PointerBindingListener pressedPtr releasedPtr
    listenerPtr <- malloc :: IO (Ptr PointerBindingListener)
    poke listenerPtr listener
    newBinding <- riverSeatGetPointerBinding seat key modifier
    _ <- wlProxyAddListener (castPtr newBinding) (castPtr listenerPtr) dataPtr

    pure state{manageQueue = manageQueue state >> riverPointerBindingEnable newBinding}
