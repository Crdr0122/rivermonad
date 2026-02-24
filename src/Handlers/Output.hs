module Handlers.Output where

import Data.IORef
import Data.Map.Strict qualified as M
import Foreign
import Foreign.C
import Types
import Wayland.ImportedFunctions

foreign export ccall "hs_output_position"
  hsOutputPosition :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
foreign export ccall "hs_output_dimensions"
  hsOutputDimensions :: Ptr () -> Ptr RiverOutput -> Int -> Int -> IO ()
foreign export ccall "hs_output_removed"
  hsOutputRemoved :: Ptr () -> Ptr RiverOutput -> IO ()
foreign export ccall "hs_output_wl_output"
  hsOutputWlOutput :: Ptr () -> Ptr RiverOutput -> CUInt -> IO ()

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

hsOutputWlOutput :: Ptr () -> Ptr RiverOutput -> CUInt -> IO ()
hsOutputWlOutput _ _ _ = pure ()

hsOutputRemoved :: Ptr () -> Ptr RiverOutput -> IO ()
hsOutputRemoved dataPtr output = do
  putStrLn "Output Destroyed"
  riverOutputDestroy output
  stateIORef <- deRefStablePtr (castPtrToStablePtr dataPtr)
  state <- readIORef stateIORef
  let lsPtr = outLayerShell $ allOutputs state M.! output
  riverLayerShellOutputDestroy lsPtr
  let newOutputs = M.delete output (allOutputs state)
      newLayerShells = M.delete lsPtr (allLayerShellOutputs state)
      newFocusedOutput
        | focusedOutput state == output = nullPtr
        | otherwise = focusedOutput state
  writeIORef
    stateIORef
    state
      { allOutputs = newOutputs
      , focusedOutput = newFocusedOutput
      , allLayerShellOutputs = newLayerShells
      }
