module Handlers.Output where

import Control.Concurrent.MVar
import Data.Bimap qualified as B
import Data.Map.Strict qualified as M
import Foreign
import Foreign.C
import Optics.Core
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
  modifyMVar_ stateMVar $ \(s :: WMState) ->
    pure $ s & #allOutputs % at output %? #outGeometry %~ \g -> g & #rw .~ width & #rh .~ height

hsOutputPosition :: Ptr () -> Ptr RiverOutput -> CInt -> CInt -> IO ()
hsOutputPosition dataPtr output x y = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(s :: WMState) ->
    pure $ s & #allOutputs % at output %? #outGeometry %~ \g -> g & #rx .~ x & #ry .~ y

hsOutputWlOutput :: Ptr () -> Ptr RiverOutput -> CUInt -> IO ()
hsOutputWlOutput _ _ _ = pure ()

hsOutputRemoved :: Ptr () -> Ptr RiverOutput -> IO ()
hsOutputRemoved dataPtr output = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> case state ^? #allOutputs % at output % _Just of
    Nothing -> pure state
    Just o -> do
      riverOutputDestroy output
      riverLayerShellOutputDestroy $ o ^. #outLayerShell
      pure $
        state
          & (#allOutputs %~ M.delete output)
          & (#allLayerShellOutputs %~ M.delete (o ^. #outLayerShell))
          & (#allOutputWorkspaces %~ B.delete output)
          & (#focusedOutput %~ (\oldO -> if oldO == output then nullPtr else oldO))
