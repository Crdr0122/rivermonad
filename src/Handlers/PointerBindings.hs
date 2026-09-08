module Handlers.PointerBindings (registerPtrbind) where

import Control.Concurrent.MVar
import Data.Set (Set)
import Optics.Core
import Protocols.Generated
import Types
import Utils.Keysyms
import Wayland.Connection

registerPtrbind :: MVar WMState -> Object RiverSeatV1 -> (PointerBtn, Set RiverSeatV1ModifiersFlag) -> (Object RiverSeatV1 -> MVar WMState -> W (), Object RiverSeatV1 -> MVar WMState -> W ()) -> W ()
registerPtrbind mvar seat (PointerBtn ptr, modifiers) (onPressed, onReleased) = do
  modifyMVarW_ mvar $ \s -> do
    let handler =
          RiverPointerBindingV1Handlers
            { onRiverPointerBindingV1Pressed = \_ -> onPressed seat mvar
            , onRiverPointerBindingV1Released = \_ -> onReleased seat mvar
            }
    newBinding <- riverSeatV1GetPointerBinding seat ptr modifiers handler
    pure $
      s
        & (#manageQueue >>~ riverPointerBindingV1Enable newBinding)
        & (#allSeats % at seat %? #seatPtrBinds %~ (newBinding :))
