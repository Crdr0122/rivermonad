{-# LANGUAGE CApiFFI #-}

module Wayland.Protocol.RiverWM where

import Control.Concurrent.MVar

-- import Data.Text qualified as T

import Foreign
import Types
import Wayland.Client

data RiverWMManager

foreign import ccall unsafe "get_river" get_river :: Ptr ()
foreign import ccall unsafe "get_river_wm_listener" get_river_wm_listener :: Ptr ()
foreign import ccall unsafe "get_river_window_listener" get_river_window_listener :: Ptr ()
foreign import ccall unsafe "get_river_output_listener" get_river_output_listener :: Ptr ()
foreign import capi "river-wm.h river_window_manager_v1_manage_finish"
  river_window_manager_v1_manage_finish :: Ptr RiverWMManager -> IO ()
foreign import capi "river-wm.h river_window_v1_fullscreen"
  river_window_v1_fullscreen :: Ptr () -> Ptr () -> IO ()

foreign export ccall "hs_on_new_window"
  hs_on_new_window :: Ptr () -> IO ()

foreign export ccall "hs_on_new_output"
  hs_on_new_output :: Ptr () -> IO ()

foreign export ccall "hs_manage_start"
  hs_manage_start :: Ptr RiverWMManager -> IO ()

hs_on_new_window :: Ptr () -> IO ()
hs_on_new_window win = do
  putStrLn "Haskell: registering new window"
  let w = Window win
  windowPtr <- newStablePtr w
  _ <- wl_proxy_add_listener (castPtr win) get_river_window_listener (castStablePtrToPtr windowPtr)
  windowsMVar <- allWindows
  modifyMVar_ windowsMVar (pure . (w :))

  outputsMVar <- allOutputs
  outputs <- readMVar outputsMVar
  applyLayout win outputs
  pure ()

hs_on_new_output :: Ptr () -> IO ()
hs_on_new_output output = do
  putStrLn "Haskell: registering new output"
  let o = Output output
  outputPtr <- newStablePtr o
  _ <- wl_proxy_add_listener (castPtr output) get_river_output_listener (castStablePtrToPtr outputPtr)
  -- Add output into list of outputs
  outputsMVar <- allOutputs
  modifyMVar_ outputsMVar (pure . (o :))
  pure ()

hs_manage_start :: Ptr RiverWMManager -> IO ()
hs_manage_start wmManager = do
  putStrLn "Haskell: finishing manager"
  actionsMVar <- allActions
  actions <- takeMVar actionsMVar
  -- sequence_ actions
  river_window_manager_v1_manage_finish wmManager

applyLayout :: Ptr () -> [Output] -> IO ()
applyLayout _ [] = pure ()
applyLayout w (o : _) = do
  actionsMVar <- allActions
  let a = river_window_v1_fullscreen w (outPtr o)
  modifyMVar_ actionsMVar (pure . (a :))
