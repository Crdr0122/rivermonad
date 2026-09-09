module Handlers.Output (mkOutputHandler) where

import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.State
import Data.Bimap qualified as B
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Optics.Core
import Optics.State
import Optics.State.Operators
import Protocols.Generated
import Types
import Utils.Helpers
import Wayland.Connection

mkOutputHandler :: MVar WMState -> RiverOutputV1Handlers
mkOutputHandler mvar =
  RiverOutputV1Handlers
    { onRiverOutputV1Dimensions = dimensions mvar
    , onRiverOutputV1Position = position mvar
    , onRiverOutputV1CaptureSessions = \_ _ -> pure ()
    , onRiverOutputV1Removed = removed mvar
    , onRiverOutputV1WlOutput = wlOutput mvar
    }

dimensions :: MVar WMState -> Object RiverOutputV1 -> Int32 -> Int32 -> W ()
dimensions mvar out width height = do
  modifyMVarW_ mvar $ pure . (#allOutputs % at out %? #outGeo %~ \g -> g & #rw .~ width & #rh .~ height)

position :: MVar WMState -> Object RiverOutputV1 -> Int32 -> Int32 -> W ()
position mvar out x y = do
  modifyMVarW_ mvar $ pure . (#allOutputs % at out %? #outGeo %~ \g -> g & #rx .~ x & #ry .~ y)

wlOutput :: MVar WMState -> Object RiverOutputV1 -> Word32 -> W ()
wlOutput mvar out wlOut = do
  modifyMVarW_ mvar $ pure . execState transform
 where
  transform = do
    #allOutputs % at out %? #outWlOut .= wlOut
    oWs <- use #allOutputWorkspaces
    use (#persistedStateOutputs % at wlOut) >>= \case
      Just oldW | B.notMemberR oldW oWs -> do
        #allOutputWorkspaces %= B.insert out oldW
      _ -> do
        let remainingWorkspace = fromMaybe 0 $ L.find (\n -> B.notMemberR n $ oWs) [1 ..]
        #allOutputWorkspaces %= B.insert out remainingWorkspace

    #persistedStateOutputs % at wlOut .= Nothing
    fO <- use #focusedOut
    when (fO == nonObject) $ #focusedOut .= out

removed :: MVar WMState -> Object RiverOutputV1 -> W ()
removed mvar out = do
  modifyMVarW_ mvar $ execStateT transform
 where
  transform = do
    lift $ riverOutputV1Destroy out
    use (#allOutputs % at out) >>= \case
      Nothing -> pure ()
      Just o -> do
        #allLayerShellOutputs %= M.delete (o ^. #outLayerShellObj)
        lift $ riverLayerShellOutputV1Destroy (o ^. #outLayerShellObj)
        #allOutputs %= M.delete out

    #allOutputWorkspaces %= B.delete out
    -- Delete first then check remaining
    use (pairOfGetter #focusedOut (#allOutputWorkspaces % to B.keys)) >>= \case
      (currentFocusedOutput, []) | currentFocusedOutput == out -> #focusedOut .= nonObject
      (currentFocusedOutput, h : _) | currentFocusedOutput == out -> #focusedOut .= h
      _ -> pure ()
