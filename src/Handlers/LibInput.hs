module Handlers.LibInput (mkLibInputHandlers) where

import Control.Concurrent.MVar
import Protocols.Generated
import Types

mkLibInputHandlers :: MVar WMState -> RiverLibinputConfigV1Handlers
mkLibInputHandlers _ =
  RiverLibinputConfigV1Handlers
    { onRiverLibinputConfigV1Finished = \c -> riverLibinputConfigV1Destroy c
    , onRiverLibinputConfigV1LibinputDevice = \_ _ -> pure $ Just deviceHandler
    }

deviceHandler :: RiverLibinputDeviceV1Handlers
deviceHandler =
  RiverLibinputDeviceV1Handlers
    { onRiverLibinputDeviceV1Removed = \d -> riverLibinputDeviceV1Destroy d
    , onRiverLibinputDeviceV1InputDevice = \_ _ -> pure ()
    , onRiverLibinputDeviceV1SendEventsSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1SendEventsDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1SendEventsCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1TapSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1TapDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1TapCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1TapButtonMapDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1TapButtonMapCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1DragDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1DragCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1DragLockDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1DragLockCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ThreeFingerDragSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ThreeFingerDragDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ThreeFingerDragCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1CalibrationMatrixSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1CalibrationMatrixDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1CalibrationMatrixCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1AccelProfilesSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1AccelProfileDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1AccelProfileCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1AccelSpeedDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1AccelSpeedCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1NaturalScrollSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1NaturalScrollDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1NaturalScrollCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1LeftHandedSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1LeftHandedDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1LeftHandedCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ClickMethodSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ClickMethodDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ClickMethodCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ClickfingerButtonMapDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ClickfingerButtonMapCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1MiddleEmulationSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1MiddleEmulationDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1MiddleEmulationCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ScrollMethodSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ScrollMethodDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ScrollMethodCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ScrollButtonDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ScrollButtonCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ScrollButtonLockDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1ScrollButtonLockCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1DwtSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1DwtDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1DwtCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1DwtpSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1DwtpDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1DwtpCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1RotationSupport = \_ _ -> pure ()
    , onRiverLibinputDeviceV1RotationDefault = \_ _ -> pure ()
    , onRiverLibinputDeviceV1RotationCurrent = \_ _ -> pure ()
    , onRiverLibinputDeviceV1Done = \_ -> pure ()
    }
