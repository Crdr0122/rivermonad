module Handlers.WlrOutput (mkWlrOutputManagerHandlers) where

import Config (myConfig)
import Control.Concurrent.MVar
import Control.Monad (forM_, unless)
import Control.Monad.Reader
import Control.Monad.State
import Data.List qualified as L
import Data.Map qualified as M
import Optics.Core
import Optics.State
import Optics.State.Operators
import Protocols.Generated
import Types
import Wayland.Connection

mkWlrOutputManagerHandlers :: MVar WMState -> ZwlrOutputManagerV1Handlers
mkWlrOutputManagerHandlers mvar =
  ZwlrOutputManagerV1Handlers
    { onZwlrOutputManagerV1Head = newHead mvar
    , onZwlrOutputManagerV1Done = managerDone mvar
    , onZwlrOutputManagerV1Finished = \_ -> modifyMVarW_ mvar $ \s -> pure $ s & #tempWlrOuts .~ M.empty
    }

mkHeadHandlers :: MVar WMState -> ZwlrOutputHeadV1Handlers
mkHeadHandlers mvar =
  ZwlrOutputHeadV1Handlers
    { onZwlrOutputHeadV1AdaptiveSync = \h sync -> modifyMVarW_ mvar $ \s -> pure $ s & #tempWlrOuts % at h %? #wlrOutSync .~ sync
    , onZwlrOutputHeadV1Name = \h name -> modifyMVarW_ mvar $ \s -> pure $ s & #tempWlrOuts % at h %? #wlrOutName .~ name
    , onZwlrOutputHeadV1SerialNumber = \h serial -> modifyMVarW_ mvar $ \s -> pure $ s & #tempWlrOuts % at h %? #wlrOutSerial .~ serial
    , onZwlrOutputHeadV1CurrentMode = \h m -> modifyMVarW_ mvar $ \s -> pure $ s & #tempWlrOuts % at h %? #wlrOutCurMode .~ m
    , onZwlrOutputHeadV1Mode = newMode mvar
    , onZwlrOutputHeadV1Description = \_ _ -> pure ()
    , onZwlrOutputHeadV1PhysicalSize = \_ _ _ -> pure ()
    , onZwlrOutputHeadV1Enabled = \_ _ -> pure ()
    , onZwlrOutputHeadV1Position = \_ _ _ -> pure ()
    , onZwlrOutputHeadV1Transform = \_ _ -> pure ()
    , onZwlrOutputHeadV1Scale = \_ _ -> pure ()
    , onZwlrOutputHeadV1Finished = \_ -> pure ()
    , onZwlrOutputHeadV1Make = \_ _ -> pure ()
    , onZwlrOutputHeadV1Model = \_ _ -> pure ()
    }

mkModeHandlers :: MVar WMState -> Object ZwlrOutputHeadV1 -> ZwlrOutputModeV1Handlers
mkModeHandlers mvar h =
  ZwlrOutputModeV1Handlers
    { onZwlrOutputModeV1Finished = \m -> do
        zwlrOutputModeV1Release m
        modifyMVarW_ mvar $ \s -> pure $ s & #tempWlrOuts % at h %? #wlrOutMode % at m .~ Nothing
    , onZwlrOutputModeV1Refresh = \m r -> modifyMVarW_ mvar $ \s -> pure $ s & #tempWlrOuts % at h %? #wlrOutMode % at m %? #modeRefresh .~ r
    , onZwlrOutputModeV1Size = \m width height -> modifyMVarW_ mvar $ \s -> pure $ s & #tempWlrOuts % at h %? #wlrOutMode % at m %? #modeSize .~ (width, height)
    , onZwlrOutputModeV1Preferred = \_ -> pure ()
    }

mkConfigHandlers :: ZwlrOutputConfigurationV1Handlers
mkConfigHandlers =
  ZwlrOutputConfigurationV1Handlers
    { onZwlrOutputConfigurationV1Cancelled = zwlrOutputConfigurationV1Destroy
    , onZwlrOutputConfigurationV1Succeeded = zwlrOutputConfigurationV1Destroy
    , onZwlrOutputConfigurationV1Failed = zwlrOutputConfigurationV1Destroy
    }

newHead :: MVar WMState -> Object ZwlrOutputManagerV1 -> Object ZwlrOutputHeadV1 -> W (Maybe ZwlrOutputHeadV1Handlers)
newHead mvar _ h = do
  modifyMVarW_ mvar $ pure . execState transform
  pure $ Just $ mkHeadHandlers mvar
 where
  transform = do
    let o =
          WlrOut
            { wlrOutObj = h
            , wlrOutCurMode = nonObject
            , wlrOutMode = M.empty
            , wlrOutName = ""
            , wlrOutSerial = ""
            , wlrOutSync = ZwlrOutputHeadV1AdaptiveSyncStateDisabled
            }
    #tempWlrOuts % at h ?= o

newMode :: MVar WMState -> Object ZwlrOutputHeadV1 -> Object ZwlrOutputModeV1 -> W (Maybe ZwlrOutputModeV1Handlers)
newMode mvar h m = do
  modifyMVarW_ mvar $ pure . execState transform
  pure $ Just $ mkModeHandlers mvar h
 where
  transform = do
    let mode =
          WlrMode
            { modeRefresh = 0
            , modeObj = m
            , modeSize = (0, 0)
            }
    #tempWlrOuts % at h %? #wlrOutMode % at m ?= mode

managerDone :: MVar WMState -> Object ZwlrOutputManagerV1 -> Word32 -> W ()
managerDone mvar m serial = do
  modifyMVarW_ mvar $ execStateT transform
 where
  transform = do
    c <- lift $ zwlrOutputManagerV1CreateConfiguration m serial mkConfigHandlers
    outs <- use (#tempWlrOuts % to M.elems)
    let rules = myConfig ^. #outputRules
    forM_ rules $ \(name, size, refresh, sync) -> do
      case L.find (\o -> wlrOutName o == name) outs of
        Nothing -> pure ()
        Just out -> do
          ch <- lift $ zwlrOutputConfigurationV1EnableHead c (wlrOutObj out) ZwlrOutputConfigurationHeadV1Handlers{}
          unless (wlrOutSync out == sync) $ (lift $ zwlrOutputConfigurationHeadV1SetAdaptiveSync ch sync)
          let modes = wlrOutMode out
          case L.find (\mode -> modeSize mode == size && modeRefresh mode == refresh) modes of
            Nothing -> pure ()
            Just WlrMode{modeObj} -> unless (wlrOutCurMode out == modeObj) $ (lift $ zwlrOutputConfigurationHeadV1SetMode ch modeObj)
    lift $ zwlrOutputConfigurationV1Apply c
    #tempWlrOuts .= M.empty
