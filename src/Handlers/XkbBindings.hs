module Handlers.XkbBindings (registerKeybind) where

import Control.Concurrent.MVar
import Data.Set (Set)
import Optics.Core
import Protocols.Generated
import Types
import Utils.Keysyms
import Wayland.Connection

registerKeybind :: MVar WMState -> Object RiverSeatV1 -> (Keysym, Set RiverSeatV1ModifiersFlag) -> (Object RiverSeatV1 -> MVar WMState -> W ()) -> W ()
registerKeybind mvar seat (Keysym key, modifiers) onPressed = do
  modifyMVarW_ mvar $ \s -> do
    let handler =
          RiverXkbBindingV1Handlers
            { onRiverXkbBindingV1Pressed = \_ -> onPressed seat mvar
            , onRiverXkbBindingV1Released = \_ -> pure ()
            , onRiverXkbBindingV1StopRepeat = \_ -> pure ()
            }
    newBinding <- riverXkbBindingsV1GetXkbBinding (s ^. #currentXkbBindings) seat key modifiers handler
    pure $
      s
        & (#manageQueue >>~ riverXkbBindingV1Enable newBinding)
        & (#allSeats % at seat %? #seatXkbBinds %~ (newBinding :))
