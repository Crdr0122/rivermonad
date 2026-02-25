module Handlers.Seat where

import Control.Monad (unless, when)
import Data.IORef
import Data.Map.Strict qualified as M
import Foreign
import Types
import Wayland.ImportedFunctions

foreign export ccall "hs_pointer_enter"
  hsPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()

foreign export ccall "hs_window_interaction"
  hsWindowInteraction :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()

hsPointerEnter :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hsPointerEnter _ _ _ = pure ()

-- hsPointerEnter dataPtr seat win = do
--   stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   state <- readIORef stateIORef
--   let focusWin = riverSeatFocusWindow seat win
--       window = (allWindows state) M.! win
--       node = nodePtr window
--   when (isFloating window) $ do
--     writeIORef
--       stateIORef
--       state
--         { focusedWindow = Just (win)
--         , manageQueue = manageQueue state >> focusWin >> riverNodePlaceTop node
--         }

hsWindowInteraction :: Ptr () -> Ptr RiverSeat -> Ptr RiverWindow -> IO ()
hsWindowInteraction dataPtr seat win = do
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
