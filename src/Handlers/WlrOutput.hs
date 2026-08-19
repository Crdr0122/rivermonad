module Handlers.WlrOutput where

import Control.Concurrent.MVar
import Control.Monad.State
import Foreign
import Foreign.C
import Types

foreign export ccall "hs_wlr_output_manager_head"
  hsWlrOutputManagerHead :: Ptr () -> Ptr WlrOutputManager -> Ptr WlrOutputHead -> IO ()
foreign export ccall "hs_wlr_output_manager_done"
  hsWlrOutputManagerDone :: Ptr () -> Ptr WlrOutputManager -> CUInt -> IO ()
foreign export ccall "hs_wlr_output_manager_finished"
  hsWlrOutputManagerFinished :: Ptr () -> Ptr WlrOutputManager -> IO ()

hsWlrOutputManagerHead :: Ptr () -> Ptr WlrOutputManager -> Ptr WlrOutputHead -> IO ()
hsWlrOutputManagerHead dataPtr _manager _outputHead = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
 where
  transform = do
    pure ()
hsWlrOutputManagerDone :: Ptr () -> Ptr WlrOutputManager -> CUInt -> IO ()
hsWlrOutputManagerDone dataPtr _manager _serial = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
 where
  transform = do
    pure ()
hsWlrOutputManagerFinished :: Ptr () -> Ptr WlrOutputManager -> IO ()
hsWlrOutputManagerFinished dataPtr _manager = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
 where
  transform = do
    pure ()
