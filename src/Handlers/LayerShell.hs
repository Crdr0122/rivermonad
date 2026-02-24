module Handlers.LayerShell where

import Data.IORef
import Data.Map.Strict qualified as M
import Foreign
import Types

foreign export ccall "hs_layer_shell_output_non_exclusive_area"
  hsLayerShellOutputNonExclusiveArea :: Ptr () -> Ptr RiverLayerShellOutput -> Int -> Int -> Int -> Int -> IO ()

hsLayerShellOutputNonExclusiveArea :: Ptr () -> Ptr RiverLayerShellOutput -> Int -> Int -> Int -> Int -> IO ()
hsLayerShellOutputNonExclusiveArea dataPtr lsOutput x y width height = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateIORef $ \state -> do
    let oldOutputs = allOutputs state
    case M.lookup lsOutput (allLayerShellOutputs state) of
      Nothing -> state
      Just oPtr -> do
        case M.lookup oPtr oldOutputs of
          Nothing -> state
          Just o -> do
            let updatedOutput = o{outX = x, outY = y, outWidth = width, outHeight = height}
                newOutputs = M.insert oPtr updatedOutput oldOutputs
            state{allOutputs = newOutputs}
