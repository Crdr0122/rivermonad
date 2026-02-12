module Main where

import Config
import Control.Monad (forever)
import Data.IORef
import Data.Map.Strict qualified as M
import Data.Sequence qualified as S
import Foreign.Ptr
import Foreign.StablePtr
import Types
import Utils.BiMap qualified as B
import Wayland.Client
import Wayland.ImportedFunctions

main :: IO ()
main = do
  display <- wlDisplayConnect nullPtr
  if display == nullPtr
    then putStrLn "Failed to connect to Wayland"
    else putStrLn "Connected to Wayland!"
  registry <- wlDisplayGetRegistry display
  if registry == nullPtr
    then putStrLn "Failed to get registry"
    else putStrLn "Got registry!"

  reg_listener <- pure getRegistryListener
  _ <- wlProxyAddListener (castPtr registry) reg_listener nullPtr

  _ <- wlDisplayRoundtrip display

  let
    comp = getCompositor
    river = getRiver
    xkbBindings = getXkbBindings

  if comp == nullPtr
    then putStrLn "Compositor NOT bound"
    else putStrLn "Compositor bound!"

  if river == nullPtr
    then putStrLn "River NOT bound"
    else putStrLn "River bound!"

  if xkbBindings == nullPtr
    then putStrLn "XKb NOT bound"
    else putStrLn "XKb bound!"

  st <-
    newIORef
      WMState
        { manageQueue = pure ()
        , renderQueue = pure ()
        , allWindows = M.empty
        , focusedWindow = Nothing
        , allOutputs = M.empty
        , focusedOutput = nullPtr
        , allWorkspaces = B.empty
        , focusedSeat = nullPtr
        , focusedWorkspace = 1
        , workspaceLayouts = M.empty
        , currentXkbBindings = xkbBindings
        }
  stPtr <- newStablePtr st

  _ <- wlProxyAddListener (castPtr river) getRiverWmListener (castStablePtrToPtr stPtr)

  _ <- wlDisplayRoundtrip display

  forever $ do
    _ <- wlDisplayRoundtrip display
    pure ()
