module Handlers.LayerShell where

import Control.Concurrent.MVar
import Foreign
import Foreign.C
import Optics.Core
import Types
import Wayland.ImportedFunctions

foreign export ccall "hs_layer_shell_output_non_exclusive_area"
  hsLayerShellOutputNonExclusiveArea :: Ptr () -> Ptr RiverLayerShellOutput -> CInt -> CInt -> CInt -> CInt -> IO ()

hsLayerShellOutputNonExclusiveArea :: Ptr () -> Ptr RiverLayerShellOutput -> CInt -> CInt -> CInt -> CInt -> IO ()
hsLayerShellOutputNonExclusiveArea dataPtr lsOutput x y width height = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> do
    case state ^. #allLayerShellOutputs % at lsOutput of
      Nothing -> pure state
      Just oPtr -> pure $ state & #allOutputs % at oPtr %? #outGeometry .~ Rect x y width height

foreign export ccall "hs_layer_shell_seat_focus_none"
  hsLayerShellSeatFocusNone :: Ptr () -> Ptr RiverLayerShellSeat -> IO ()

hsLayerShellSeatFocusNone :: Ptr () -> Ptr RiverLayerShellSeat -> IO ()
hsLayerShellSeatFocusNone dataPtr _ = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(s :: WMState) ->
    pure $ s & #manageQueue %~ (>> mapM_ (riverSeatFocusWindow (s ^. #focusedSeat)) (s ^? #focusedWindow % _Just))
