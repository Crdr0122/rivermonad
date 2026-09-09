module Handlers.LibInput (mkLibInputHandlers) where

import Control.Concurrent.MVar
import Protocols.Generated
import Types

mkLibInputHandlers :: MVar WMState -> RiverLibinputConfigV1Handlers
mkLibInputHandlers _ =
  RiverLibinputConfigV1Handlers
    { onRiverLibinputConfigV1Finished = \c -> riverLibinputConfigV1Destroy c
    , onRiverLibinputConfigV1LibinputDevice = \_ _ -> pure Nothing
    }
