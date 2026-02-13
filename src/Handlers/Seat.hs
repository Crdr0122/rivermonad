module Handlers.Seat where

import Data.IORef
import Data.Map.Strict qualified as M
import Foreign
import Types
import Wayland.ImportedFunctions

foreign export ccall "hs_pointer_enter"
  hsPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()

hsPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hsPointerEnter dataPtr seat win = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  let focusWin = riverSeatFocusWindow seat win
      window = (allWindows state) M.! win
      node = nodePtr window
      raiseNode
        | isFloating window = riverNodePlaceTop node
        | otherwise = pure ()
  writeIORef
    stateIORef
    state
      { focusedWindow = Just (win)
      , manageQueue = manageQueue state >> focusWin >> raiseNode
      }
