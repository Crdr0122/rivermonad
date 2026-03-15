module Handlers.XkbBindings where

import Control.Concurrent.MVar
import Data.Map.Strict (adjust)
import Foreign
import Foreign.C
import Types
import Wayland.Client
import Wayland.ImportedFunctions

data XkbBindingListener = XkbBindingListener
  { xkbPressed :: FunPtr XkbCallback
  , xkbReleased :: FunPtr XkbCallback
  , xkbStopRepeat :: FunPtr XkbCallback
  }

instance Storable XkbBindingListener where
  sizeOf _ = sizeOf (nullPtr :: Ptr ()) * 3
  alignment _ = alignment (nullPtr :: Ptr ())
  poke ptr (XkbBindingListener p r s) = do
    let pSize = sizeOf (nullPtr :: Ptr ())
    poke (ptr `plusPtr` (pSize * 0)) p
    poke (ptr `plusPtr` (pSize * 1)) r
    poke (ptr `plusPtr` (pSize * 2)) s
  peek ptr = do
    let offset = sizeOf (nullPtr :: Ptr ())
    pressed <- peek (castPtr ptr) :: IO (FunPtr XkbCallback)
    released <- peekByteOff ptr offset :: IO (FunPtr XkbCallback)
    stopRepeat <- peekByteOff ptr (offset * 2) :: IO (FunPtr XkbCallback)
    pure $ XkbBindingListener pressed released stopRepeat

foreign import ccall "wrapper"
  mkXkbCallback :: XkbCallback -> IO (FunPtr XkbCallback)

registerKeybind :: Ptr () -> Ptr RiverSeat -> ((CUInt, CUInt), (MVar WMState -> IO ())) -> IO ()
registerKeybind dataPtr seat ((key, modifier), (onPressed)) = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    pressedPtr <- mkXkbCallback (\d _ -> deRefStablePtr (castPtrToStablePtr d) >>= onPressed)
    releasedPtr <- mkXkbCallback (\_ _ -> pure ())
    stopRepeatPtr <- mkXkbCallback (\_ _ -> pure ())

    let listener = XkbBindingListener pressedPtr releasedPtr stopRepeatPtr
        bindingManager = currentXkbBindings state
    listenerPtr <- malloc :: IO (Ptr XkbBindingListener)
    poke listenerPtr listener
    newBinding <- riverXkbBindingsGetXkbBinding bindingManager seat key modifier
    _ <- wlProxyAddListener (castPtr newBinding) (castPtr listenerPtr) dataPtr

    pure
      state
        { manageQueue = manageQueue state >> riverXkbBindingEnable newBinding
        , seatXkbBindings = adjust (newBinding :) seat (seatXkbBindings state)
        }
