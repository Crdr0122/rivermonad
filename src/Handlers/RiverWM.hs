module Handlers.RiverWM where

import Config

import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader
import Handlers.PointerBindings
import Handlers.XkbBindings
import Layout
import Optics.Core
import Protocols.Generated
import Types
import Wayland.Connection

mkRiverWMHandler :: MVar WMState -> RiverWindowManagerV1Handlers
mkRiverWMHandler mvar =
  RiverWindowManagerV1Handlers
    { onRiverWindowManagerV1Finished = \_ -> pure ()
    , onRiverWindowManagerV1Unavailable = \_ -> liftIO $ putStrLn "River unavailable, another WM running"
    , onRiverWindowManagerV1ManageStart = \_ -> pure ()
    , onRiverWindowManagerV1RenderStart = renderStart mvar
    , onRiverWindowManagerV1SessionLocked = sessionLocked mvar
    , onRiverWindowManagerV1SessionUnlocked = sessionUnlocked mvar
    , onRiverWindowManagerV1Window = newWindow mvar
    , onRiverWindowManagerV1Seat = newSeat mvar
    , onRiverWindowManagerV1Output = newOutput mvar
    }

-- foreign export ccall "hs_wm_window"
--   hsWmWindow :: Ptr () -> Ptr RiverWMManager -> Ptr RiverWindow -> IO ()
-- foreign export ccall "hs_wm_output"
--   hsWmOutput :: Ptr () -> Ptr RiverWMManager -> Ptr RiverOutput -> IO ()
-- foreign export ccall "hs_wm_seat"
--   hsWmSeat :: Ptr () -> Ptr RiverWMManager -> Ptr RiverSeat -> IO ()
-- foreign export ccall "hs_wm_manage_start"
--   hsWmManageStart :: Ptr () -> Ptr RiverWMManager -> IO ()
-- foreign export ccall "hs_wm_render_start"
--   hsWmRenderStart :: Ptr () -> Ptr RiverWMManager -> IO ()
-- foreign export ccall "hs_wm_session_locked"
--   hsWmSessionLocked :: Ptr () -> Ptr RiverWMManager -> IO ()
-- foreign export ccall "hs_wm_session_unlocked"
--   hsWmSessionUnlocked :: Ptr () -> Ptr RiverWMManager -> IO ()
--
newWindow :: MVar WMState -> Object RiverWindowManagerV1 -> Object RiverWindowV1 -> W (Maybe RiverWindowV1Handlers)
newWindow mvar wm win = do
  pure Nothing
newOutput :: MVar WMState -> Object RiverWindowManagerV1 -> Object RiverOutputV1 -> W (Maybe RiverOutputV1Handlers)
newOutput mvar wm out = do
  pure Nothing
newSeat :: MVar WMState -> Object RiverWindowManagerV1 -> Object RiverSeatV1 -> W (Maybe RiverSeatV1Handlers)
newSeat mvar wm seat = do
  pure Nothing

-- hsWmWindow dataPtr _ win = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ stateMVar $ \(s :: WMState) -> do
--     node <- riverWindowGetNode win
--     _ <- wlProxyAddListener (castPtr win) getRiverWindowListener dataPtr
--     let w =
--           Window
--             { winPtr = win
--             , nodePtr = node
--             , isFloating = False
--             , isFullscreen = False
--             , isPinned = False
--             , isMaximized = False
--             , winIdentifier = ""
--             , winTitle = ""
--             , winAppID = ""
--             , floatingGeometry = Nothing
--             , tilingGeometry = Nothing
--             , ruleSize = Nothing
--             , dimensionsHint = (0, 0, 0, 0)
--             , parentWindow = Nothing
--             }
--     pure $ s & (#allWindows % at win ?~ w) & (#manageQueue <>~ startupApplyManage win)
--
-- hsWmSeat :: Ptr () -> Ptr RiverWMManager -> Ptr RiverSeat -> IO ()
-- hsWmSeat dataPtr _ seat = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ stateMVar $ \(state :: WMState) -> do
--     _ <- wlProxyAddListener (castPtr seat) getRiverSeatListener dataPtr
--     newLayerShellSeatPtr <- riverLayerShellGetSeat (state ^. #currentLayerShell) seat
--     _ <- wlProxyAddListener (castPtr newLayerShellSeatPtr) getRiverLayerShellSeatListener dataPtr
--     withCString (myConfig ^. #xCursorTheme % _1) $ \theme ->
--       riverSeatSetXcursorTheme seat theme (myConfig ^. #xCursorTheme % _2)
--     let s =
--           Seat
--             { seatPtr = seat
--             , seatName = 0
--             , xkbBindings = []
--             , pointerBindings = []
--             }
--     pure $ state & #focusedSeat .~ seat & #allSeats % at' seat ?~ s
--   itraverseOf_ (#allKeyBindings % itraversed) (registerKeybind dataPtr seat) myConfig
--   itraverseOf_ (#allPointerBindings % itraversed) (registerPointerbind dataPtr seat) myConfig
--
-- hsWmOutput :: Ptr () -> Ptr RiverWMManager -> Ptr RiverOutput -> IO ()
-- hsWmOutput dataPtr _ output = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ stateMVar $ \(state :: WMState) -> do
--     _ <- wlProxyAddListener (castPtr output) getRiverOutputListener dataPtr
--     newLayerShellOutputPtr <- riverLayerShellGetOutput (state ^. #currentLayerShell) output
--     _ <- wlProxyAddListener (castPtr newLayerShellOutputPtr) getRiverLayerShellOutputListener dataPtr
--     let o =
--           Output
--             { outPtr = output
--             , outLayerShell = newLayerShellOutputPtr
--             , outGeometry = (Rect 0 0 0 0)
--             , outWlOutput = 0
--             }
--     pure $
--       state
--         & (#allOutputs % at' output ?~ o)
--         & (#allLayerShellOutputs % at' newLayerShellOutputPtr ?~ output)
--
-- hsWmManageStart :: Ptr () -> Ptr RiverWMManager -> IO ()
-- hsWmManageStart dataPtr wm = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   startLayout stateMVar
--   riverWindowManagerManageFinish wm
--
renderStart :: MVar WMState -> Object RiverWindowManagerV1 -> W ()
renderStart mvar wm = do
  modifyMVarW_ mvar $ \s -> do
    s ^. #renderQueue
    riverWindowManagerV1RenderFinish wm
    pure $ s & #renderQueue .~ pure ()

--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ stateMVar $ \(s :: WMState) -> do
--     s ^. #renderQueue
--     riverWindowManagerRenderFinish wm
--     pure $ s & #renderQueue .~ pure ()
--
sessionLocked :: MVar WMState -> Object RiverWindowManagerV1 -> W ()
sessionLocked mvar _ = do
  modifyMVarW_ mvar $ \s ->
    pure $
      s
        & #manageQueue
        >>~ ( traverseOf_ (#allSeats % traversed % #seatXkbBinds % traversed) riverXkbBindingV1Disable s
                >> traverseOf_ (#allSeats % traversed % #seatPtrBinds % traversed) riverPointerBindingV1Disable s
            )

sessionUnlocked :: MVar WMState -> Object RiverWindowManagerV1 -> W ()
sessionUnlocked mvar _ = do
  modifyMVarW_ mvar $ \s ->
    pure $
      s
        & #manageQueue
        >>~ ( traverseOf_ (#allSeats % traversed % #seatXkbBinds % traversed) riverXkbBindingV1Enable s
                >> traverseOf_ (#allSeats % traversed % #seatPtrBinds % traversed) riverPointerBindingV1Enable s
            )

-- hsWmSessionLocked :: Ptr () -> Ptr RiverWMManager -> IO ()
-- hsWmSessionLocked dataPtr _ = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ stateMVar $ \(state :: WMState) ->
--     pure $
--       state
--         & #manageQueue
--         <>~ ( traverseOf_ (#allSeats % traversed % #xkbBindings % traversed) riverXkbBindingDisable state
--                 >> traverseOf_ (#allSeats % traversed % #pointerBindings % traversed) riverPointerBindingDisable state
--             )
--
-- hsWmSessionUnlocked :: Ptr () -> Ptr RiverWMManager -> IO ()
-- hsWmSessionUnlocked dataPtr _ = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ stateMVar $ \(state :: WMState) ->
--     pure $
--       state
--         & #manageQueue
--         <>~ ( traverseOf_ (#allSeats % traversed % #xkbBindings % traversed) riverXkbBindingEnable state
--                 >> traverseOf_ (#allSeats % traversed % #pointerBindings % traversed) riverPointerBindingEnable state
--             )
--
startupApplyManage :: Object RiverWindowV1 -> W ()
startupApplyManage w = do
  let use_ssd = riverWindowV1UseSsd w
      set_tiled =
        riverWindowV1SetTiled
          w
          [ RiverWindowV1EdgesTop
          , RiverWindowV1EdgesLeft
          , RiverWindowV1EdgesRight
          , RiverWindowV1EdgesBottom
          ]
  set_tiled >> use_ssd

--
-- startupApplyRender :: Ptr RiverWindow -> Ptr RiverNode -> IO ()
-- startupApplyRender _ _ = pure ()
