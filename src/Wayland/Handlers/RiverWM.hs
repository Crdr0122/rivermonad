module Wayland.Handlers.RiverWM where

-- import Data.Text qualified as T

import Data.IORef
import Data.Map.Strict qualified as M
import Data.Sequence qualified as S
import Foreign
import Layout
import Types
import Utils.BiMap qualified as B
import Wayland.Client
import Wayland.Protocol.ImportedFunctions

foreign export ccall "hs_on_new_window"
  hsOnNewWindow :: Ptr () -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_on_new_output"
  hsOnNewOutput :: Ptr () -> Ptr RiverOutput -> IO ()
foreign export ccall "hs_on_new_seat"
  hsOnNewSeat :: Ptr () -> Ptr RiverSeat -> IO ()
foreign export ccall "hs_manage_start"
  hsManageStart :: Ptr () -> Ptr RiverWMManager -> IO ()
foreign export ccall "hs_render_start"
  hsRenderStart :: Ptr () -> Ptr RiverWMManager -> IO ()

hsOnNewWindow :: Ptr () -> Ptr RiverWindow -> IO ()
hsOnNewWindow dataPtr win = do
  node <- riverWindowGetNode win
  let w =
        Window
          { winPtr = win
          , nodePtr = node
          , isFloating = False
          , isFullscreen = False
          }
  _ <- wlProxyAddListener (castPtr win) getRiverWindowListener dataPtr
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateIORef $ \state -> do
    let newWindowsList = M.insert win w (allWindows state)
        newManageQueue = manageQueue state >> (startupApplyManage win)
        newWorkspaces = B.insert (focusedWorkspace state) win (allWorkspaces state)
    state
      { allWindows = newWindowsList
      , manageQueue = newManageQueue
      , focusedWindow = Just (win)
      , allWorkspaces = newWorkspaces
      }

hsOnNewSeat :: Ptr () -> Ptr RiverSeat -> IO ()
hsOnNewSeat dataPtr seat = do
  _ <- wlProxyAddListener (castPtr seat) getRiverSeatListener dataPtr
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  pressedPtr <- mkXkbCallback onPressed
  releasedPtr <- mkXkbCallback (\_ _ -> putStrLn "Released")
  stopRepeatPtr <- mkXkbCallback (\_ _ -> return ())
  let listener = XkbBindingListener pressedPtr releasedPtr stopRepeatPtr
  listenerPtr <- malloc :: IO (Ptr XkbBindingListener)
  poke listenerPtr listener
  let bindingManager = currentXkbBindings state
  newBinding <- riverXkbBindingsGetXkbBinding bindingManager seat 113 8
  _ <- wlProxyAddListener (castPtr newBinding) (castPtr listenerPtr) dataPtr
  writeIORef stateIORef state{focusedSeat = seat}

hsOnNewOutput :: Ptr () -> Ptr RiverOutput -> IO ()
hsOnNewOutput dataPtr output = do
  let o = Output output 0 0 0 0
  _ <- wlProxyAddListener (castPtr output) getRiverOutputListener dataPtr
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateMVar $ \state -> do
    let newOutputsList = M.insert output o (allOutputs state)
    state{allOutputs = newOutputsList, focusedOutput = output}

hsManageStart :: Ptr () -> Ptr RiverWMManager -> IO ()
hsManageStart dataPtr wmManager = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  manageQueue state
  renderActions <- startLayout state
  riverWindowManagerManageFinish wmManager
  writeIORef
    stateIORef
    state
      { manageQueue = return ()
      , renderQueue = renderQueue state >> renderActions
      }

hsRenderStart :: Ptr () -> Ptr RiverWMManager -> IO ()
hsRenderStart dataPtr wmManager = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  renderQueue state
  riverWindowManagerRenderFinish wmManager
  writeIORef stateIORef state{renderQueue = return ()}

startupApplyManage :: Ptr RiverWindow -> IO ()
startupApplyManage w = do
  let use_ssd = riverWindowUseSsd w
      set_tiled = riverWindowSetTiled w edgeBottom
  set_tiled >> use_ssd

startupApplyRender :: Ptr RiverWindow -> Ptr RiverNode -> IO ()
startupApplyRender _ _ = pure ()

type XkbCallback = Ptr () -> Ptr RiverXkbBinding -> IO ()
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

onPressed :: Ptr () -> Ptr RiverXkbBinding -> IO ()
onPressed dataPtr _ = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  putStrLn "Key pressed! Updating state..."
