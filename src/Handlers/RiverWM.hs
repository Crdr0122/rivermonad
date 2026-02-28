module Handlers.RiverWM where

import Config

-- import Data.Bits ((.|.))
import Data.IORef
import Data.Map.Strict qualified as M
import Foreign
import Foreign.C
import Handlers.PointerBindings
import Handlers.XkbBindings
import Layout
import Types
import Utils.BiSeqMap qualified as BS
import Wayland.Client
import Wayland.ImportedFunctions

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
          , floatingGeometry = Nothing
          }
  _ <- wlProxyAddListener (castPtr win) getRiverWindowListener dataPtr
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateIORef $ \state -> do
    let newWindowsList = M.insert win w (allWindows state)
        newManageQueue = manageQueue state >> (startupApplyManage win)
        newWorkspacesTiled = BS.insert (focusedWorkspace state) win (allWorkspacesTiled state)
    state
      { allWindows = newWindowsList
      , manageQueue = newManageQueue
      , focusedWindow = Just (win)
      , allWorkspacesTiled = newWorkspacesTiled
      }

hsOnNewSeat :: Ptr () -> Ptr RiverSeat -> IO ()
hsOnNewSeat dataPtr seat = do
  _ <- wlProxyAddListener (castPtr seat) getRiverSeatListener dataPtr
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  writeIORef stateIORef state{focusedSeat = seat}
  theme <- newCString (fst xCursorTheme)
  riverSeatSetXcursorTheme seat theme (snd xCursorTheme)
  mapM_ (registerKeybind dataPtr seat) allKeyBindings
  mapM_ (registerPointerbind dataPtr seat) allPointerBindings

hsOnNewOutput :: Ptr () -> Ptr RiverOutput -> IO ()
hsOnNewOutput dataPtr output = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  newLayerShellOutputPtr <- riverLayerShellGetOutput (currentLayerShell state) output
  let o = Output output newLayerShellOutputPtr 0 0 0 0
      newOutputsList = M.insert output o (allOutputs state)
      newLayerShellOutputs = M.insert newLayerShellOutputPtr output (allLayerShellOutputs state)
  _ <- wlProxyAddListener (castPtr output) getRiverOutputListener dataPtr
  _ <- wlProxyAddListener (castPtr newLayerShellOutputPtr) getRiverLayerShellOutputListener dataPtr
  riverLayerShellOutputSetDefault newLayerShellOutputPtr
  writeIORef
    stateIORef
    state
      { allOutputs = newOutputsList
      , focusedOutput = output
      , allLayerShellOutputs = newLayerShellOutputs
      }

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
  writeIORef stateIORef state{renderQueue = pure ()}

startupApplyManage :: Ptr RiverWindow -> IO ()
startupApplyManage w = do
  let use_ssd = riverWindowUseSsd w
      set_tiled = riverWindowSetTiled w 15
  set_tiled >> use_ssd

startupApplyRender :: Ptr RiverWindow -> Ptr RiverNode -> IO ()
startupApplyRender _ _ = pure ()
