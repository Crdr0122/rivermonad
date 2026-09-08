module Handlers.LayerShell (mkLayerShellOutputHandler, mkLayerShellSeatHandler) where

import Control.Concurrent.MVar
import Optics.Core
import Protocols.Generated
import Types
import Utils.Helpers
import Wayland.Connection

mkLayerShellOutputHandler :: MVar WMState -> RiverLayerShellOutputV1Handlers
mkLayerShellOutputHandler mvar =
  RiverLayerShellOutputV1Handlers
    { onRiverLayerShellOutputV1NonExclusiveArea = nonExclusiveArea mvar
    }

nonExclusiveArea :: MVar WMState -> Object RiverLayerShellOutputV1 -> Int32 -> Int32 -> Int32 -> Int32 -> W ()
nonExclusiveArea mvar lsOut x y w h =
  modifyMVarW_ mvar $ \s -> do
    case s ^. #allLayerShellOutputs % at lsOut of
      Nothing -> pure s
      Just out -> pure $ s & #allOutputs % at out %? #outGeo .~ Rect x y w h

mkLayerShellSeatHandler :: MVar WMState -> RiverLayerShellSeatV1Handlers
mkLayerShellSeatHandler mvar =
  RiverLayerShellSeatV1Handlers
    { onRiverLayerShellSeatV1FocusExclusive = focusExclusive mvar
    , onRiverLayerShellSeatV1FocusNonExclusive = focusNonExclusive mvar
    , onRiverLayerShellSeatV1FocusNone = focusNone mvar
    }

focusExclusive :: MVar WMState -> Object RiverLayerShellSeatV1 -> W ()
focusExclusive mvar _ = do
  modifyMVarW_ mvar $ \s -> pure $ s & #focusedWin .~ Nothing

focusNonExclusive :: MVar WMState -> Object RiverLayerShellSeatV1 -> W ()
focusNonExclusive mvar _ = do
  modifyMVarW_ mvar $ \s -> pure $ s & #focusedWin .~ Nothing

focusNone :: MVar WMState -> Object RiverLayerShellSeatV1 -> W ()
focusNone mvar _ = do
  modifyMVarW_ mvar $ \s -> do
    if s ^. #focusedOut == nonObject
      then
        pure $ s & #focusedWin .~ Nothing
      else case s ^. focusedWorkspace of
        Nothing -> pure $ s & #focusedWin .~ Nothing
        Just ws -> case s ^. #workspaceFocusHistory % at ws of
          Nothing -> pure $ s & #focusedWin .~ Nothing
          Just w -> pure $ s & #focusedWin ?~ w
