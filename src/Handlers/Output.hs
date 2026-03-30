module Handlers.Output where

import Control.Concurrent.MVar
import Control.Monad (when)
import Control.Monad.State hiding (state)
import Data.Bimap qualified as B
import Data.List qualified as L
import Data.Map.Strict qualified as M
import Data.Maybe
import Foreign
import Foreign.C
import Optics.Core
import Optics.State
import Optics.State.Operators
import Types
import Utils.Helpers
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
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . (#allOutputs % at output %? #outGeometry %~ \g -> g & #rw .~ width & #rh .~ height)

hsOutputPosition :: Ptr () -> Ptr RiverOutput -> CInt -> CInt -> IO ()
hsOutputPosition dataPtr output x y = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . (#allOutputs % at output %? #outGeometry %~ \g -> g & #rx .~ x & #ry .~ y)

hsOutputWlOutput :: Ptr () -> Ptr RiverOutput -> CUInt -> IO ()
hsOutputWlOutput dataPtr output wlOutput = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
 where
  transform = do
    #allOutputs % at output %? #outWlOutput .= wlOutput
    oWs <- use #allOutputWorkspaces
    use (#persistedStateOutputs % at (cuintToWord32 wlOutput)) >>= \case
      Just oldW | B.notMemberR oldW oWs -> do
        #allOutputWorkspaces %= B.insert output oldW
        #persistedStateOutputs % at (cuintToWord32 wlOutput) .= Nothing
      _ -> do
        let remainingWorkspace = fromMaybe 0 $ L.find (\n -> B.notMemberR n $ oWs) [1 ..]
        #allOutputWorkspaces %= B.insert output remainingWorkspace

    fO <- use #focusedOutput
    when (fO == nullPtr) $ #focusedOutput .= output

hsOutputRemoved :: Ptr () -> Ptr RiverOutput -> IO ()
hsOutputRemoved dataPtr output = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \(state :: WMState) -> do
    riverOutputDestroy output
    traverseOf_ (#allOutputs % at output %? #outLayerShell) riverLayerShellOutputDestroy state
    pure $ execState transform state
 where
  transform = do
    #allOutputs %= M.delete output
    #allOutputWorkspaces %= B.delete output

    use (pairOfGetter #focusedOutput (#allOutputWorkspaces % to B.keys)) >>= \case
      (oldO, []) | oldO /= output -> #focusedOutput .= nullPtr
      (oldO, h : _) | oldO /= output -> #focusedOutput .= h
      _ -> pure ()

    use (#allOutputs % at output) >>= \case
      Nothing -> pure ()
      Just o -> #allLayerShellOutputs %= M.delete (o ^. #outLayerShell)
