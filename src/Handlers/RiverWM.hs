module Handlers.RiverWM where

import Config

import Control.Concurrent.MVar
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
    pure $ s & (#allWindows % at win ?~ w) & (#manageQueue <>~ startupApplyManage win)

hsWmSeat :: Ptr () -> Ptr RiverWMManager -> Ptr RiverSeat -> IO ()
hsWmSeat dataPtr _ seat = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> do
    _ <- wlProxyAddListener (castPtr seat) getRiverSeatListener dataPtr
    newLayerShellSeatPtr <- riverLayerShellGetSeat (state ^. #currentLayerShell) seat
    _ <- wlProxyAddListener (castPtr newLayerShellSeatPtr) getRiverLayerShellSeatListener dataPtr
    theme <- newCString (myConfig ^. #xCursorTheme % _1)
    riverSeatSetXcursorTheme seat theme (myConfig ^. #xCursorTheme % _2)
    let s =
          Seat
            { seatPtr = seat
            , seatName = 0
            , xkbBindings = []
            , pointerBindings = []
            }
    pure $ state & #focusedSeat .~ seat & #allSeats % at' seat ?~ s
  itraverseOf_ (#allKeyBindings % itraversed) (registerKeybind dataPtr seat) myConfig
  itraverseOf_ (#allPointerBindings % itraversed) (registerPointerbind dataPtr seat) myConfig

hsWmOutput :: Ptr () -> Ptr RiverWMManager -> Ptr RiverOutput -> IO ()
hsWmOutput dataPtr _ output = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> do
    _ <- wlProxyAddListener (castPtr output) getRiverOutputListener dataPtr
    newLayerShellOutputPtr <- riverLayerShellGetOutput (state ^. #currentLayerShell) output
    _ <- wlProxyAddListener (castPtr newLayerShellOutputPtr) getRiverLayerShellOutputListener dataPtr
    let o =
          Output
            { outPtr = output
            , outLayerShell = newLayerShellOutputPtr
            , outGeometry = (Rect 0 0 0 0)
            , outWlOutput = 0
            }
    pure $
      state
        & (#allOutputs % at' output ?~ o)
        & (#allLayerShellOutputs % at' newLayerShellOutputPtr ?~ output)
        & (#manageQueue <>~ riverLayerShellOutputSetDefault newLayerShellOutputPtr)

hsWmManageStart :: Ptr () -> Ptr RiverWMManager -> IO ()
hsWmManageStart dataPtr wm = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  startLayout stateMVar
  riverWindowManagerManageFinish wm

hsWmRenderStart :: Ptr () -> Ptr RiverWMManager -> IO ()
hsWmRenderStart dataPtr wm = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(s :: WMState) -> do
    s ^. #renderQueue
    riverWindowManagerRenderFinish wm
    pure $ s & #renderQueue .~ pure ()

hsWmSessionLocked :: Ptr () -> Ptr RiverWMManager -> IO ()
hsWmSessionLocked dataPtr _ = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) ->
    pure $
      state
        & #manageQueue
        <>~ ( traverseOf_ (#allSeats % traversed % #xkbBindings % traversed) riverXkbBindingDisable state
                >> traverseOf_ (#allSeats % traversed % #pointerBindings % traversed) riverPointerBindingDisable state
            )

hsWmSessionUnlocked :: Ptr () -> Ptr RiverWMManager -> IO ()
hsWmSessionUnlocked dataPtr _ = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) ->
    pure $
      state
        & #manageQueue
        <>~ ( traverseOf_ (#allSeats % traversed % #xkbBindings % traversed) riverXkbBindingEnable state
                >> traverseOf_ (#allSeats % traversed % #pointerBindings % traversed) riverPointerBindingEnable state
                >> traverseOf_ (#focusedWindow % _Just) (riverSeatFocusWindow (state ^. #focusedSeat)) state
            )

startupApplyManage :: Ptr RiverWindow -> IO ()
startupApplyManage w = do
  let use_ssd = riverWindowUseSsd w
      set_tiled = riverWindowSetTiled w edgeAll
  set_tiled >> use_ssd

startupApplyRender :: Ptr RiverWindow -> Ptr RiverNode -> IO ()
startupApplyRender _ _ = pure ()
