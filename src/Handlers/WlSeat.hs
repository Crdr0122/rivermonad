module Handlers.WlSeat (mkWlSeatHandlers) where

import Control.Concurrent.MVar
import Control.Monad.Reader
import Control.Monad.State
import Data.Set qualified as S
import Optics.Core
import Optics.State
import Optics.State.Operators
import Protocols.Generated
import Types
import Wayland.Connection
import Wayland.Generated

mkWlSeatHandlers :: MVar WMState -> Word32 -> WlSeatHandlers
mkWlSeatHandlers mvar name =
  WlSeatHandlers
    { onWlSeatCapabilities = seatCapa mvar name
    , onWlSeatName = \_ _ -> pure ()
    }

seatCapa :: MVar WMState -> Word32 -> Object WlSeat -> S.Set WlSeatCapabilityFlag -> W ()
seatCapa mvar name wlSeat capabilities = do
  modifyMVarW_ mvar $ execStateT transform
 where
  transform = do
    #allWlSeats % at name %? #wlSeatCapabilities .= capabilities
    let hasPointer = WlSeatCapabilityPointer `S.member` capabilities
    if hasPointer
      then do
        pointer <- lift $ wlSeatGetPointer wlSeat (ptrHandler mvar name)

        cursorManager <- use #currentCursorShapeManager
        device <- lift $ wpCursorShapeManagerV1GetPointer cursorManager pointer WpCursorShapeDeviceV1Handlers{}
        #allWlSeats % at name %? #wlCursorShapeDevice ?= device
        #allWlSeats % at name %? #wlPointer ?= pointer
      else do
        #allWlSeats % at name %? #wlPointer .= Nothing
        #allWlSeats % at name %? #wlCursorShapeDevice .= Nothing

        preuse (#allWlSeats % at name %? #wlCursorShapeDevice % _Just) >>= \case
          Nothing -> pure ()
          Just pointer -> lift $ wpCursorShapeDeviceV1Destroy pointer
        preuse (#allWlSeats % at name %? #wlPointer % _Just) >>= \case
          Nothing -> pure ()
          Just pointer -> lift $ wlPointerRelease pointer

ptrHandler :: MVar WMState -> Word32 -> WlPointerHandlers
ptrHandler mvar name =
  WlPointerHandlers
    { onWlPointerEnter = \_ serial _ _ _ -> modifyMVarW_ mvar $ \s -> pure $ s & #allWlSeats % at name %? #wlPointerSerial .~ serial
    , onWlPointerLeave = \_ _ _ -> pure ()
    , onWlPointerMotion = \_ _ _ _ -> pure ()
    , onWlPointerButton = \_ _ _ _ _ -> pure ()
    , onWlPointerAxis = \_ _ _ _ -> pure ()
    , onWlPointerFrame = \_ -> pure ()
    , onWlPointerAxisSource = \_ _ -> pure ()
    , onWlPointerAxisStop = \_ _ _ -> pure ()
    , onWlPointerAxisDiscrete = \_ _ _ -> pure ()
    , onWlPointerAxisValue120 = \_ _ _ -> pure ()
    , onWlPointerAxisRelativeDirection = \_ _ _ -> pure ()
    , onWlPointerWarp = \_ _ _ -> pure ()
    }
