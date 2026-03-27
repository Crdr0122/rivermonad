module Handlers.WlSeat where

import Control.Concurrent.MVar
import Control.Monad (forM_, msum, when)
import Control.Monad.State hiding (state)
import Data.Bimap qualified as B
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Foreign
import Foreign.C
import Optics.Core
import Optics.State
import Optics.State.Operators
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.Client
import Wayland.ImportedFunctions

wlSeatCapabilitiesPointer :: CUInt
wlSeatCapabilitiesPointer = 1
wlSeatCapabilitiesKeyboard :: CUInt
wlSeatCapabilitiesKeyboard = 2
wlSeatCapabilitiesTouch :: CUInt
wlSeatCapabilitiesTouch = 4

foreign export ccall "hs_wl_seat_capabilities"
  hsWlSeatCapabilities :: Ptr () -> Ptr WlSeat -> CUInt -> IO ()

foreign export ccall "hs_wl_seat_name"
  hsWlSeatName :: Ptr () -> Ptr WlSeat -> CString -> IO ()

hsWlSeatCapabilities :: Ptr () -> Ptr WlSeat -> CUInt -> IO ()
hsWlSeatCapabilities dataPtr wlSeat capabilities = do
  (stateMVar, name) <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) ->
    if capabilities .&. wlSeatCapabilitiesPointer /= 0
      then do
        pointer <- wlSeatGetPointer wlSeat
        _ <- wlProxyAddListener (castPtr pointer) getWlPointerListener dataPtr
        -- cursorDevice <- cursorShapeManagerGetPointer (state ^. ) pointer
        pure $
          state
            & (#allWlSeats % at name %? #wlPointer .~ pointer)
            & (#allWlSeats % at name %? #wlSeatCapabilities .~ capabilities)
      else pure $ state & #allWlSeats % at name %? #wlSeatCapabilities .~ capabilities

hsWlSeatName :: Ptr () -> Ptr WlSeat -> CString -> IO ()
hsWlSeatName _ _ _ = pure ()

foreign export ccall "hs_wl_pointer_enter"
  hsWlPointerEnter :: Ptr () -> Ptr WlPointer -> CUInt -> Ptr () -> WlFixedT -> WlFixedT -> IO ()

hsWlPointerEnter :: Ptr () -> Ptr WlPointer -> CUInt -> Ptr () -> WlFixedT -> WlFixedT -> IO ()
hsWlPointerEnter dataPtr _ serial _ _ _ = do
  (stateMVar, name) <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> do
    pure $ state & #allWlSeats % at name %? #wlPointerSerial .~ serial
