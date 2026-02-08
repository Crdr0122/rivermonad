module Main where

import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.Map qualified as M
import Foreign.Ptr
import Foreign.StablePtr
import Types
import Wayland.Client
import Wayland.Protocol.ImportedFunctions

main :: IO ()
main = do
  display <- wl_display_connect nullPtr
  if display == nullPtr
    then putStrLn "Failed to connect to Wayland"
    else putStrLn "Connected to Wayland!"
  registry <- wl_display_get_registry display
  if registry == nullPtr
    then putStrLn "Failed to get registry"
    else putStrLn "Got registry!"

  mQueue <- newMVar (pure ())
  rQueue <- newMVar (pure ())
  focusedO <- newEmptyMVar
  w <- newMVar M.empty
  o <- newMVar M.empty
  st <- newStablePtr (WMState mQueue rQueue w o focusedO)

  reg_listener <- pure get_registry_listener
  _ <- wl_proxy_add_listener (castPtr registry) reg_listener nullPtr

  _ <- wl_display_roundtrip display

  comp <- pure get_compositor
  if comp == nullPtr
    then putStrLn "Compositor NOT bound"
    else putStrLn "Compositor bound!"

  river <- pure get_river
  if river == nullPtr
    then putStrLn "River NOT bound"
    else putStrLn "River bound!"

  listener <- pure get_river_wm_listener
  _ <- wl_proxy_add_listener (castPtr river) listener (castStablePtrToPtr st)

  _ <- wl_display_roundtrip display

  forever $ do
    _ <- wl_display_dispatch display
    pure ()
