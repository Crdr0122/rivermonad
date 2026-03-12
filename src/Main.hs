module Main where

import Config
import Control.Concurrent.MVar
import Control.Monad (forever)
import Data.Aeson
import Data.Bimap qualified as B
import Data.ByteString.Lazy qualified as Byte
import Data.Map.Strict qualified as M
import Foreign.Ptr
import Foreign.StablePtr
import System.Directory
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Utils.KeyDispatches
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
    layerShell = getLayerShell
    xkbConfig = getXkbConfig

  if comp == nullPtr
    then putStrLn "Compositor NOT bound"
    else putStrLn "Compositor bound!"

  if river == nullPtr
    then putStrLn "River NOT bound"
    else putStrLn "River bound!"

  if xkbBindings == nullPtr
    then putStrLn "XKb NOT bound"
    else putStrLn "XKb bound!"

  exists <- doesFileExist (statePath myConfig)
  (ratios, oldWindows) <-
    if not exists
      then pure (defaultRatios myConfig, M.empty)
      else do
        content <- Byte.readFile (statePath myConfig)
        case decode content of
          Just PersistedState{persistedWorkspaceRatios, persistedWindows} -> do
            removeFile (statePath myConfig)
            pure (persistedWorkspaceRatios, persistedWindows)
          Nothing -> pure (defaultRatios myConfig, M.empty)

  fd <- createKeymapFd (composeKeyMap myConfig)
  st <-
    newMVar
      WMState
        { manageQueue = pure ()
        , renderQueue = pure ()
        , allWindows = M.empty
        , focusedWindow = Nothing
        , allOutputs = M.empty
        , allLayerShellOutputs = M.empty
        , focusedOutput = nullPtr
        , allWorkspacesTiled = BS.fromList (zip [1 .. 9] (repeat []))
        , allWorkspacesFloating = BS.fromList (zip [1 .. 9] (repeat []))
        , allWorkspacesFullscreen = BS.fromList (zip [1 .. 9] (repeat []))
        , floatingQueue = M.fromList (zip [1 .. 9] (repeat []))
        , fullscreenQueue = M.fromList (zip [1 .. 9] (repeat []))
        , newWindowQueue = []
        , focusedSeat = nullPtr
        , allOutputWorkspaces = B.empty
        , lastFocusedWorkspace = 1
        , workspaceLayouts = defaultLayouts myConfig
        , workspaceRatios = ratios
        , currentWmManager = river
        , currentXkbBindings = xkbBindings
        , currentLayerShell = layerShell
        , currentXkbConfig = xkbConfig
        , opDeltaState = None
        , currentOpDelta = (0, 0, 0, 0)
        , cursorPosition = (0, 0)
        , persistedState = oldWindows
        , currentKeymap = fd
        }
  stPtr <- newStablePtr st

  _ <- wlProxyAddListener (castPtr river) getRiverWmListener (castStablePtrToPtr stPtr)
  _ <- wlProxyAddListener (castPtr xkbConfig) getRiverXkbConfigListener (castStablePtrToPtr stPtr)

  _ <- wlDisplayRoundtrip display

  mapM_ (flip exec st) (execOnStart myConfig)

  forever $ do
    _ <- wlDisplayDispatch display
    pure ()
