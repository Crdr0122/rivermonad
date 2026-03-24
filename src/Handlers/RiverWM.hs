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
import Optics.Core
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
  modifyMVar_ stateMVar $ \(s :: WMState) -> do
    node <- riverWindowGetNode win
    _ <- wlProxyAddListener (castPtr win) getRiverWindowListener dataPtr
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
    pure $ s & #allWindows % at win ?~ w & #manageQueue %~ (>> startupApplyManage win)

hsWmSeat :: Ptr () -> Ptr RiverWMManager -> Ptr RiverSeat -> IO ()
hsWmSeat dataPtr _ seat = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> do
    _ <- wlProxyAddListener (castPtr seat) getRiverSeatListener dataPtr
    newLayerShellSeatPtr <- riverLayerShellGetSeat (state ^. #currentLayerShell) seat
    _ <- wlProxyAddListener (castPtr newLayerShellSeatPtr) getRiverLayerShellSeatListener dataPtr
    theme <- newCString (myConfig ^. #xCursorTheme % _1)
    riverSeatSetXcursorTheme seat theme (myConfig ^. #xCursorTheme % _2)
    pure $ state & #focusedSeat .~ seat & #seatXkbBindings % at' seat ?~ [] & #seatPointerBindings % at' seat ?~ []

  mapM_ (registerKeybind dataPtr seat) (myConfig ^. #allKeyBindings % to M.toList)
  mapM_ (registerPointerbind dataPtr seat) (myConfig ^. #allPointerBindings % to M.toList)

hsWmOutput :: Ptr () -> Ptr RiverWMManager -> Ptr RiverOutput -> IO ()
hsWmOutput dataPtr _ output = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> do
    newLayerShellOutputPtr <- riverLayerShellGetOutput (currentLayerShell state) output
    _ <- wlProxyAddListener (castPtr output) getRiverOutputListener dataPtr
    _ <- wlProxyAddListener (castPtr newLayerShellOutputPtr) getRiverLayerShellOutputListener dataPtr
    let o = Output output newLayerShellOutputPtr (Rect 0 0 0 0)
        remainingWorkspace = fromMaybe 0 $ L.find (\n -> B.notMemberR n $ state ^. #allOutputWorkspaces) [1 ..]
        newOutputsWorkspaces = B.insert output remainingWorkspace (allOutputWorkspaces state)
    pure $
      state
        & (#allOutputs % at' output ?~ o)
        & (#allLayerShellOutputs % at' newLayerShellOutputPtr ?~ output)
        & (#focusedOutput .~ output)
        & (#manageQueue %~ (>> riverLayerShellOutputSetDefault newLayerShellOutputPtr))
        & (#allOutputWorkspaces .~ newOutputsWorkspaces)

hsWmManageStart :: Ptr () -> Ptr RiverWMManager -> IO ()
hsWmManageStart dataPtr wmManager = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  startLayout stateMVar
  riverWindowManagerManageFinish wmManager

hsWmRenderStart :: Ptr () -> Ptr RiverWMManager -> IO ()
hsWmRenderStart dataPtr wmManager = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(s :: WMState) -> do
    s ^. #renderQueue
    riverWindowManagerRenderFinish wmManager
    pure $ s & #renderQueue .~ pure ()

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
