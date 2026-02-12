module Handlers.Seat where

import Data.IORef
import Data.Map.Strict qualified as M
import Foreign
import Layout
import Types
import Wayland.Client
import Wayland.ImportedFunctions

foreign export ccall "hs_pointer_enter"
  hsPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()

hsPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hsPointerEnter dataPtr seat win = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  let focusWin = riverSeatFocusWindow seat win
      node = nodePtr ((allWindows state) M.! win)
      raiseNode = riverNodePlaceTop node
  writeIORef
    stateIORef
    state
      { focusedWindow = Just (win)
      , manageQueue = manageQueue state >> focusWin >> raiseNode
      }
