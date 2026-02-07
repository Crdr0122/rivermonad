module Main where

import Foreign.Ptr
import Wayland.Client
import Wayland.Protocol.RiverWM

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

  listener <- pure get_registry_listener
  _ <- wl_proxy_add_listener (castPtr registry) listener nullPtr

  _ <- wl_display_roundtrip display
  putStrLn "Roundtrip complete!"

  comp <- pure get_compositor
  if comp == nullPtr
    then putStrLn "Compositor NOT bound"
    else putStrLn "Compositor bound!"

  river <- pure get_river
  if river == nullPtr
    then putStrLn "River NOT bound"
    else putStrLn "River bound!"
