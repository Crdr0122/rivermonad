module Handlers.RiverWM where

import Config

import Control.Concurrent.MVar
import Control.Monad.IO.Class (liftIO)
import Handlers.LayerShell
import Handlers.Output
import Handlers.PointerBindings
import Handlers.Seat
import Handlers.Window
import Handlers.XkbBindings
import Layout
import Optics.Core
import Protocols.Generated
import Types
import Utils.Helpers
import Wayland.Connection

mkRiverWMHandler :: MVar WMState -> RiverWindowManagerV1Handlers
mkRiverWMHandler mvar =
  RiverWindowManagerV1Handlers
    { onRiverWindowManagerV1Finished = \_ -> pure ()
    , onRiverWindowManagerV1Unavailable = \_ -> liftIO $ putStrLn "River unavailable, another WM running"
    , onRiverWindowManagerV1ManageStart = manageStart mvar
    , onRiverWindowManagerV1RenderStart = renderStart mvar
    , onRiverWindowManagerV1SessionLocked = sessionLocked mvar
    , onRiverWindowManagerV1SessionUnlocked = sessionUnlocked mvar
    , onRiverWindowManagerV1Window = newWindow mvar
    , onRiverWindowManagerV1Seat = newSeat mvar
    , onRiverWindowManagerV1Output = newOutput mvar
    }

newWindow :: MVar WMState -> Object RiverWindowManagerV1 -> Object RiverWindowV1 -> W (Maybe RiverWindowV1Handlers)
newWindow mvar _ win = do
  node <- riverWindowV1GetNode win (RiverNodeV1Handlers{})
  let w =
        Window
          { winObj = win
          , winNodeObj = node
          , winFloat = False
          , winFull = False
          , winPinned = False
          , winMaximized = False
          , winIdentifier = ""
          , winTitle = ""
          , winAppId = ""
          , winFloatGeo = Nothing
          , winTileGeo = Nothing
          , winSizeRule = Nothing
          , winDimHint = (0, 0, 0, 0)
          , winParent = Nothing
          }
  modifyMVarW_ mvar $ \s -> do
    pure $ s & (#allWindows % at win ?~ w) & (#manageQueue >>~ startupApplyManage win)
  pure $ Just $ mkWindowHandler mvar

newOutput :: MVar WMState -> Object RiverWindowManagerV1 -> Object RiverOutputV1 -> W (Maybe RiverOutputV1Handlers)
newOutput mvar _ out = do
  modifyMVarW_ mvar $ \s -> do
    ls <- riverLayerShellV1GetOutput (s ^. #currentLayerShell) out (mkLayerShellOutputHandler mvar)
    let o =
          Output
            { outObj = out
            , outLayerShellObj = ls
            , outGeo = (Rect 0 0 0 0)
            , outWlOut = 0
            }
    pure $
      s
        & (#allOutputs % at' out ?~ o)
        & (#allLayerShellOutputs % at' ls ?~ out)
  pure $ Just $ mkOutputHandler mvar

newSeat :: MVar WMState -> Object RiverWindowManagerV1 -> Object RiverSeatV1 -> W (Maybe RiverSeatV1Handlers)
newSeat mvar _ seat = do
  modifyMVarW_ mvar $ \s -> do
    _ <- riverLayerShellV1GetSeat (s ^. #currentLayerShell) seat (mkLayerShellSeatHandler mvar)
    riverSeatV1SetXcursorTheme seat (myConfig ^. #xCursorTheme % _1) (myConfig ^. #xCursorTheme % _2)
    let sRec =
          Seat
            { seatObj = seat
            , seatWlSeat = 0
            , seatXkbBinds = []
            , seatPtrBinds = []
            }
    pure $ s & #focusedSeat .~ seat & #allSeats % at' seat ?~ sRec
  itraverseOf_ (#allKeyBindings % itraversed) (registerKeybind mvar seat) myConfig
  itraverseOf_ (#allPointerBindings % itraversed) (registerPtrbind mvar seat) myConfig
  pure $ Just $ mkSeatHandler mvar

manageStart :: MVar WMState -> Object RiverWindowManagerV1 -> W ()
manageStart mvar wm = do
  startLayout mvar
  riverWindowManagerV1ManageFinish wm

renderStart :: MVar WMState -> Object RiverWindowManagerV1 -> W ()
renderStart mvar wm = do
  modifyMVarW_ mvar $ \s -> do
    s ^. #renderQueue
    riverWindowManagerV1RenderFinish wm
    pure $ s & #renderQueue .~ pure ()

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

startupApplyManage :: Object RiverWindowV1 -> W ()
startupApplyManage w = do
  let use_ssd = riverWindowV1UseSsd w
      set_tiled = riverWindowV1SetTiled w allEdges
  set_tiled >> use_ssd

--
-- startupApplyRender :: Object RiverWindowV1 -> Object RiverNodeV1 -> W ()
-- startupApplyRender _ _ = pure ()
