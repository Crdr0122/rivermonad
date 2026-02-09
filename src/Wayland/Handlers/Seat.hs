module Wayland.Handlers.Seat where

import Data.IORef
import Data.Map qualified as M
import Foreign
import Layout
import Types
import Wayland.Client
import Wayland.Protocol.ImportedFunctions

foreign export ccall "hs_pointer_enter"
  hsPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()

hsPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hsPointerEnter dataPtr seat win = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  let focusWin = riverSeatFocusWindow seat win
  modifyIORef stateIORef $ \state ->
    state
      { focusedWindow = win
      , manageQueue = manageQueue state >> focusWin
      }
