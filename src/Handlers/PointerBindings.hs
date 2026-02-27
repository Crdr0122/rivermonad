module Handlers.PointerBindings where

import Control.Monad (when)
import Data.IORef
import Data.Map qualified as M
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

registerKeybind :: Ptr () -> Ptr RiverSeat -> (CUInt, CUInt, IORef WMState -> IO (), IORef WMState -> IO ()) -> IO ()
registerKeybind dataPtr seat (key, modifier, onPressed, onReleased) = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef

  pressedPtr <- mkPointerCallback (\d _ -> deRefStablePtr (castPtrToStablePtr d) >>= onPressed)
  releasedPtr <- mkPointerCallback (\d _ -> deRefStablePtr (castPtrToStablePtr d) >>= onReleased)

  let listener = PointerBindingListener pressedPtr releasedPtr
      bindingManager = currentXkbBindings state
  listenerPtr <- malloc :: IO (Ptr PointerBindingListener)
  poke listenerPtr listener
  newBinding <- riverXkbBindingsGetXkbBinding bindingManager seat key modifier
  _ <- wlProxyAddListener (castPtr newBinding) (castPtr listenerPtr) dataPtr

  writeIORef stateIORef state{manageQueue = manageQueue state >> riverXkbBindingEnable newBinding}

dragWindow :: IORef WMState -> IO ()
dragWindow stateIORef = do
  state <- readIORef stateIORef
  let needToDrag = case focusedWindow state of
        Nothing -> False
        Just w -> isFloating (allWindows state M.! w)
  when needToDrag $ do
    riverSeatOpStartPointer (focusedSeat state)
    writeIORef stateIORef state{draggingWindow = needToDrag}

stopDragging :: IORef WMState -> IO ()
stopDragging stateIORef = do
  state <- readIORef stateIORef
  let stop = riverSeatOpEnd (focusedSeat state)
  writeIORef stateIORef state{manageQueue = manageQueue state >> stop}
