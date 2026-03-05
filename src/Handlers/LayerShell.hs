module Handlers.LayerShell where

import Control.Concurrent.MVar
import Data.Map.Strict qualified as M
import Foreign
import Foreign.C
import Types

foreign export ccall "hs_layer_shell_output_non_exclusive_area"
  hsLayerShellOutputNonExclusiveArea :: Ptr () -> Ptr RiverLayerShellOutput -> CInt -> CInt -> CInt -> CInt -> IO ()

hsLayerShellOutputNonExclusiveArea :: Ptr () -> Ptr RiverLayerShellOutput -> CInt -> CInt -> CInt -> CInt -> IO ()
hsLayerShellOutputNonExclusiveArea dataPtr lsOutput x y width height = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let oldOutputs = allOutputs state
    case M.lookup lsOutput (allLayerShellOutputs state) of
      Nothing -> pure state
      Just oPtr -> do
        case M.lookup oPtr oldOutputs of
          Nothing -> pure state
          Just o -> do
            let updatedOutput = o{outX = x, outY = y, outWidth = width, outHeight = height}
                newOutputs = M.insert oPtr updatedOutput oldOutputs
            pure state{allOutputs = newOutputs}
