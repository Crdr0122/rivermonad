module Handlers.XkbBindings where

import Config
import Data.IORef
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

foreign import ccall "wrapper"
  mkXkbCallback :: XkbCallback -> IO (FunPtr XkbCallback)

registerKeybind :: Ptr () -> Ptr RiverSeat -> ((CUInt, CUInt), IORef WMState -> IO ()) -> IO ()
registerKeybind dataPtr seat ((key, modifier), f) = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef

  let onPressed = (\d _ -> deRefStablePtr (castPtrToStablePtr d) >>= f)

  pressedPtr <- mkXkbCallback onPressed
  releasedPtr <- mkXkbCallback (\_ _ -> putStrLn "Released")
  stopRepeatPtr <- mkXkbCallback (\_ _ -> return ())

  let listener = XkbBindingListener pressedPtr releasedPtr stopRepeatPtr
      bindingManager = currentXkbBindings state
  listenerPtr <- malloc :: IO (Ptr XkbBindingListener)
  poke listenerPtr listener
  newBinding <- riverXkbBindingsGetXkbBinding bindingManager seat key modifier
  _ <- wlProxyAddListener (castPtr newBinding) (castPtr listenerPtr) dataPtr

  writeIORef stateIORef state{manageQueue = manageQueue state >> riverXkbBindingEnable newBinding}

