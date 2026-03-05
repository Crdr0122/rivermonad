module Handlers.Output where

import Control.Concurrent.MVar
import Data.Bimap qualified as B
import Data.Map.Strict qualified as M
import Foreign
import Foreign.C
import Types
import Wayland.ImportedFunctions

foreign export ccall "hs_output_position"
  hsOutputPosition :: Ptr () -> Ptr RiverOutput -> CInt -> CInt -> IO ()
foreign export ccall "hs_output_dimensions"
  hsOutputDimensions :: Ptr () -> Ptr RiverOutput -> CInt -> CInt -> IO ()
foreign export ccall "hs_output_removed"
  hsOutputRemoved :: Ptr () -> Ptr RiverOutput -> IO ()
foreign export ccall "hs_output_wl_output"
  hsOutputWlOutput :: Ptr () -> Ptr RiverOutput -> CUInt -> IO ()

hsOutputDimensions :: Ptr () -> Ptr RiverOutput -> CInt -> CInt -> IO ()
hsOutputDimensions dataPtr output width height = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let oldOutputs = allOutputs state
    case M.lookup output oldOutputs of
      Nothing -> pure state
      Just o -> do
        let updatedOutput = o{outWidth = width, outHeight = height}
            newOutputs = M.insert output updatedOutput oldOutputs
        pure state{allOutputs = newOutputs}

hsOutputPosition :: Ptr () -> Ptr RiverOutput -> CInt -> CInt -> IO ()
hsOutputPosition dataPtr output x y = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let oldOutputs = allOutputs state
    case M.lookup output oldOutputs of
      Nothing -> pure state
      Just o -> do
        let updatedOutput = o{outX = x, outY = y}
            newOutputs = M.insert output updatedOutput oldOutputs
        pure state{allOutputs = newOutputs}

hsOutputWlOutput :: Ptr () -> Ptr RiverOutput -> CUInt -> IO ()
hsOutputWlOutput _ _ _ = pure ()

hsOutputRemoved :: Ptr () -> Ptr RiverOutput -> IO ()
hsOutputRemoved dataPtr output = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    putStrLn "Output Destroyed"
    riverOutputDestroy output
    let lsPtr = outLayerShell $ allOutputs state M.! output
    riverLayerShellOutputDestroy lsPtr
    let newOutputs = M.delete output (allOutputs state)
        newLayerShells = M.delete lsPtr (allLayerShellOutputs state)
        newOutputWorkspaces = B.delete output (allOutputWorkspaces state)
        newFocusedOutput
          | focusedOutput state == output = nullPtr
          | otherwise = focusedOutput state
    pure
      state
        { allOutputs = newOutputs
        , focusedOutput = newFocusedOutput
        , allLayerShellOutputs = newLayerShells
        , allOutputWorkspaces = newOutputWorkspaces
        }
