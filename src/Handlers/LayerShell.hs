module Handlers.LayerShell where

import Control.Concurrent.MVar
import Foreign
import Foreign.C
import Optics.Core
import Types
import Utils.Helpers

foreign export ccall "hs_layer_shell_output_non_exclusive_area"
  hsLayerShellOutputNonExclusiveArea :: Ptr () -> Ptr RiverLayerShellOutput -> CInt -> CInt -> CInt -> CInt -> IO ()

hsLayerShellOutputNonExclusiveArea :: Ptr () -> Ptr RiverLayerShellOutput -> CInt -> CInt -> CInt -> CInt -> IO ()
hsLayerShellOutputNonExclusiveArea dataPtr lsOutput x y width height = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) ->
    case state ^. #allLayerShellOutputs % at lsOutput of
      Nothing -> pure state
      Just oPtr -> pure $ state & #allOutputs % at oPtr %? #outGeometry .~ Rect x y width height

foreign export ccall "hs_layer_shell_seat_focus_none"
  hsLayerShellSeatFocusNone :: Ptr () -> Ptr RiverLayerShellSeat -> IO ()

hsLayerShellSeatFocusNone :: Ptr () -> Ptr RiverLayerShellSeat -> IO ()
hsLayerShellSeatFocusNone dataPtr _ = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(s :: WMState) -> do
    if s ^. #focusedOutput == nullPtr
      then
        pure $ s & #focusedWindow .~ Nothing
      else case s ^. focusedWorkspace of
        Nothing -> pure $ s & #focusedWindow .~ Nothing
        Just ws -> case s ^. #workspaceFocusHistory % at ws of
          Nothing -> pure $ s & #focusedWindow .~ Nothing
          Just w -> pure $ s & #focusedWindow ?~ w

foreign export ccall "hs_layer_shell_seat_focus_exclusive"
  hsLayerShellSeatFocusExclusive :: Ptr () -> Ptr RiverLayerShellSeat -> IO ()

hsLayerShellSeatFocusExclusive :: Ptr () -> Ptr RiverLayerShellSeat -> IO ()
hsLayerShellSeatFocusExclusive dataPtr _ = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  -- Note if ClearFocus clears focus from layer shells, need testing later
  modifyMVar_ stateMVar $ \(s :: WMState) -> pure $ s & #focusedWindow .~ Nothing

foreign export ccall "hs_layer_shell_seat_focus_non_exclusive"
  hsLayerShellSeatFocusNonExclusive :: Ptr () -> Ptr RiverLayerShellSeat -> IO ()

hsLayerShellSeatFocusNonExclusive :: Ptr () -> Ptr RiverLayerShellSeat -> IO ()
hsLayerShellSeatFocusNonExclusive dataPtr _ = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(s :: WMState) -> pure $ s & #focusedWindow .~ Nothing
