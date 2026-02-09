module Main where

import Control.Monad (forever)
import Data.IORef
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

  st <-
    newIORef
      WMState
        { manageQueue = pure ()
        , renderQueue = pure ()
        , allWindows = M.empty
        , focusedWindow = nullPtr
        , allOutputs = M.empty
        , focusedOutput = nullPtr
        , allWorkspaces = M.empty
        , currentSeat = nullPtr
        , focusedWorkspace = 1
        }
  stPtr <- newStablePtr st

  reg_listener <- pure get_registry_listener
  _ <- wl_proxy_add_listener (castPtr registry) reg_listener nullPtr

  _ <- wl_display_roundtrip display

  comp <- pure get_compositor
  if comp == nullPtr
    then putStrLn "Compositor NOT bound"
    else putStrLn "Compositor bound!"

  river <- pure getRiver
  if river == nullPtr
    then putStrLn "River NOT bound"
    else putStrLn "River bound!"

  wmListener <- pure getRiverWmListener
  _ <- wl_proxy_add_listener (castPtr river) wmListener (castStablePtrToPtr stPtr)

  _ <- wl_display_roundtrip display

  forever $ do
    _ <- wl_display_dispatch display
    pure ()
