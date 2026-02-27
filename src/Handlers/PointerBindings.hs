module Handlers.PointerBindings where

import Data.IORef
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

registerPointerbind :: Ptr () -> Ptr RiverSeat -> (CUInt, CUInt, IORef WMState -> IO (), IORef WMState -> IO ()) -> IO ()
registerPointerbind dataPtr seat (key, modifier, onPressed, onReleased) = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef

  pressedPtr <- mkPointerCallback (\d _ -> deRefStablePtr (castPtrToStablePtr d) >>= onPressed)
  releasedPtr <- mkPointerCallback (\d _ -> deRefStablePtr (castPtrToStablePtr d) >>= onReleased)

  let listener = PointerBindingListener pressedPtr releasedPtr
  listenerPtr <- malloc :: IO (Ptr PointerBindingListener)
  poke listenerPtr listener
  newBinding <- riverSeatGetPointerBinding seat key modifier
  _ <- wlProxyAddListener (castPtr newBinding) (castPtr listenerPtr) dataPtr

  writeIORef stateIORef state{manageQueue = manageQueue state >> riverPointerBindingEnable newBinding}
