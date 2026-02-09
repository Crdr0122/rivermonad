module Wayland.Handlers.Output where

-- import Data.Text qualified as T

import Data.IORef
import Data.Map.Strict qualified as M
import Foreign
import Types
import Wayland.Client
import Wayland.Protocol.ImportedFunctions

foreign export ccall "hs_output_position"
  hsOutputPosition :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
foreign export ccall "hs_output_dimensions"
  hsOutputDimensions :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()

hsOutputDimensions :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
hsOutputDimensions dataPtr output width height = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateIORef $ \state -> do
    let oldOutputs = allOutputs state
    case M.lookup output oldOutputs of
      Nothing -> state
      Just o -> do
        let updatedOutput = o{outWidth = width, outHeight = height}
            newOutputs = M.insert output updatedOutput oldOutputs
        state{allOutputs = newOutputs}

hsOutputPosition :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
hsOutputPosition dataPtr output x y = do
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyIORef stateIORef $ \state -> do
    let oldOutputs = allOutputs state
    case M.lookup output oldOutputs of
      Nothing -> state
      Just o -> do
        let updatedOutput = o{outX = x, outY = y}
            newOutputs = M.insert output updatedOutput oldOutputs
        state{allOutputs = newOutputs}
