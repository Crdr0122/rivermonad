module Handlers.RiverWM where

import Config

import Control.Concurrent.MVar
import Data.Bimap qualified as B
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Foreign
import Foreign.C
import Handlers.PointerBindings
import Handlers.XkbBindings
import Layout
import Types
import Wayland.Client
import Wayland.ImportedFunctions

foreign export ccall "hs_on_new_window"
  hsOnNewWindow :: Ptr () -> Ptr RiverWMManager -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_on_new_output"
  hsOnNewOutput :: Ptr () -> Ptr RiverWMManager -> Ptr RiverOutput -> IO ()
foreign export ccall "hs_on_new_seat"
  hsOnNewSeat :: Ptr () -> Ptr RiverWMManager -> Ptr RiverSeat -> IO ()
foreign export ccall "hs_wm_manage_start"
  hsManageStart :: Ptr () -> Ptr RiverWMManager -> IO ()
foreign export ccall "hs_wm_render_start"
  hsRenderStart :: Ptr () -> Ptr RiverWMManager -> IO ()

hsOnNewWindow :: Ptr () -> Ptr RiverWMManager -> Ptr RiverWindow -> IO ()
hsOnNewWindow dataPtr _ win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    node <- riverWindowGetNode win
    let w =
          Window
            { winPtr = win
            , nodePtr = node
            , isFloating = False
            , isFullscreen = False
            , winIdentifier = ""
            , winTitle = ""
            , winAppID = ""
            , floatingGeometry = Nothing
            , tilingGeometry = Nothing
            , dimensionsHint = (0, 0, 0, 0)
            , parentWindow = Nothing
            }
    _ <- wlProxyAddListener (castPtr win) getRiverWindowListener dataPtr

    let newWindows = M.insert win w (allWindows state)
        newManageQueue = manageQueue state >> (startupApplyManage win)
    pure
      state
        { allWindows = newWindows
        , manageQueue = newManageQueue
        }

hsOnNewSeat :: Ptr () -> Ptr RiverWMManager -> Ptr RiverSeat -> IO ()
hsOnNewSeat dataPtr _ seat = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    _ <- wlProxyAddListener (castPtr seat) getRiverSeatListener dataPtr

    newLayerShellSeatPtr <- riverLayerShellGetSeat (currentLayerShell state) seat
    _ <- wlProxyAddListener (castPtr newLayerShellSeatPtr) getRiverLayerShellSeatListener dataPtr
    theme <- newCString (fst xCursorTheme)
    riverSeatSetXcursorTheme seat theme (snd xCursorTheme)
    pure state{focusedSeat = seat}
  mapM_ (registerKeybind dataPtr seat) allKeyBindings
  mapM_ (registerPointerbind dataPtr seat) allPointerBindings

hsOnNewOutput :: Ptr () -> Ptr RiverWMManager -> Ptr RiverOutput -> IO ()
hsOnNewOutput dataPtr _ output = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    newLayerShellOutputPtr <- riverLayerShellGetOutput (currentLayerShell state) output
    let o = Output output newLayerShellOutputPtr 0 0 0 0
        newOutputsList = M.insert output o (allOutputs state)
        newLayerShellOutputs = M.insert newLayerShellOutputPtr output (allLayerShellOutputs state)
        remainingWorkspace = fromMaybe 0 $ L.find (\n -> B.notMemberR n $ allOutputWorkspaces state) [1 ..]
        newOutputsWorkspaces = B.insert output remainingWorkspace (allOutputWorkspaces state)
    _ <- wlProxyAddListener (castPtr output) getRiverOutputListener dataPtr
    _ <- wlProxyAddListener (castPtr newLayerShellOutputPtr) getRiverLayerShellOutputListener dataPtr
    pure
      state
        { allOutputs = newOutputsList
        , focusedOutput = output
        , allLayerShellOutputs = newLayerShellOutputs
        , manageQueue = manageQueue state >> riverLayerShellOutputSetDefault newLayerShellOutputPtr
        , allOutputWorkspaces = newOutputsWorkspaces
        }

hsManageStart :: Ptr () -> Ptr RiverWMManager -> IO ()
hsManageStart dataPtr wmManager = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readMVar stateMVar
  manageQueue state
  startLayout stateMVar
  riverWindowManagerManageFinish wmManager

hsRenderStart :: Ptr () -> Ptr RiverWMManager -> IO ()
hsRenderStart dataPtr wmManager = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    renderQueue state
    riverWindowManagerRenderFinish wmManager
    pure state{renderQueue = pure ()}

startupApplyManage :: Ptr RiverWindow -> IO ()
startupApplyManage w = do
  let use_ssd = riverWindowUseSsd w
      set_tiled = riverWindowSetTiled w 15
  set_tiled >> use_ssd

startupApplyRender :: Ptr RiverWindow -> Ptr RiverNode -> IO ()
startupApplyRender _ _ = pure ()
