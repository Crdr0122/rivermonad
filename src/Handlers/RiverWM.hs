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

foreign export ccall "hs_wm_window"
  hsWmWindow :: Ptr () -> Ptr RiverWMManager -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_wm_output"
  hsWmOutput :: Ptr () -> Ptr RiverWMManager -> Ptr RiverOutput -> IO ()
foreign export ccall "hs_wm_seat"
  hsWmSeat :: Ptr () -> Ptr RiverWMManager -> Ptr RiverSeat -> IO ()
foreign export ccall "hs_wm_manage_start"
  hsWmManageStart :: Ptr () -> Ptr RiverWMManager -> IO ()
foreign export ccall "hs_wm_render_start"
  hsWmRenderStart :: Ptr () -> Ptr RiverWMManager -> IO ()
foreign export ccall "hs_wm_session_locked"
  hsWmSessionLocked :: Ptr () -> Ptr RiverWMManager -> IO ()
foreign export ccall "hs_wm_session_unlocked"
  hsWmSessionUnlocked :: Ptr () -> Ptr RiverWMManager -> IO ()

hsWmWindow :: Ptr () -> Ptr RiverWMManager -> Ptr RiverWindow -> IO ()
hsWmWindow dataPtr _ win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    node <- riverWindowGetNode win
    let w =
          Window
            { winPtr = win
            , nodePtr = node
            , isFloating = False
            , isFullscreen = False
            , isPinned = False
            , isMaximized = False
            , winIdentifier = ""
            , winTitle = ""
            , winAppID = ""
            , floatingGeometry = Nothing
            , tilingGeometry = Nothing
            , dimensionsHint = (0, 0, 0, 0)
            , parentWindow = Nothing
            }
    _ <- wlProxyAddListener (castPtr win) getRiverWindowListener dataPtr

    pure
      state
        { allWindows = M.insert win w (allWindows state)
        , manageQueue = manageQueue state >> (startupApplyManage win)
        }

hsWmSeat :: Ptr () -> Ptr RiverWMManager -> Ptr RiverSeat -> IO ()
hsWmSeat dataPtr _ seat = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    _ <- wlProxyAddListener (castPtr seat) getRiverSeatListener dataPtr

    newLayerShellSeatPtr <- riverLayerShellGetSeat (currentLayerShell state) seat
    _ <- wlProxyAddListener (castPtr newLayerShellSeatPtr) getRiverLayerShellSeatListener dataPtr
    theme <- newCString (fst (xCursorTheme myConfig))
    riverSeatSetXcursorTheme seat theme (snd (xCursorTheme myConfig))
    pure
      state
        { focusedSeat = seat
        , seatXkbBindings = M.insert seat [] (seatXkbBindings state)
        , seatPointerBindings = M.insert seat [] (seatPointerBindings state)
        }
  mapM_ (registerKeybind dataPtr seat) (M.toList $ allKeyBindings myConfig)
  mapM_ (registerPointerbind dataPtr seat) (M.toList $ allPointerBindings myConfig)

hsWmOutput :: Ptr () -> Ptr RiverWMManager -> Ptr RiverOutput -> IO ()
hsWmOutput dataPtr _ output = do
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

hsWmManageStart :: Ptr () -> Ptr RiverWMManager -> IO ()
hsWmManageStart dataPtr wmManager = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readMVar stateMVar
  manageQueue state
  startLayout stateMVar
  riverWindowManagerManageFinish wmManager

hsWmRenderStart :: Ptr () -> Ptr RiverWMManager -> IO ()
hsWmRenderStart dataPtr wmManager = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    renderQueue state
    riverWindowManagerRenderFinish wmManager
    pure state{renderQueue = pure ()}

hsWmSessionLocked :: Ptr () -> Ptr RiverWMManager -> IO ()
hsWmSessionLocked dataPtr _ = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let deActivateX = mapM_ riverXkbBindingDisable (concat $ M.elems $ seatXkbBindings state)
        deActivateP = mapM_ riverPointerBindingDisable (concat $ M.elems $ seatPointerBindings state)
    pure state{manageQueue = manageQueue state >> deActivateX >> deActivateP}

hsWmSessionUnlocked :: Ptr () -> Ptr RiverWMManager -> IO ()
hsWmSessionUnlocked dataPtr wm = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let activateX = mapM_ riverXkbBindingEnable (concat $ M.elems $ seatXkbBindings state)
        activateP = mapM_ riverPointerBindingEnable (concat $ M.elems $ seatPointerBindings state)
        focus = case focusedWindow state of
          Nothing -> pure ()
          Just w -> riverSeatFocusWindow (focusedSeat state) w
    riverWindowManagerManageDirty wm
    pure state{manageQueue = manageQueue state >> activateX >> activateP >> focus}

startupApplyManage :: Ptr RiverWindow -> IO ()
startupApplyManage w = do
  let use_ssd = riverWindowUseSsd w
      set_tiled = riverWindowSetTiled w edgeAll
  set_tiled >> use_ssd

startupApplyRender :: Ptr RiverWindow -> Ptr RiverNode -> IO ()
startupApplyRender _ _ = pure ()
