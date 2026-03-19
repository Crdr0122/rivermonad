module Main where

import Config
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Monad (forever, void)
import Control.Monad.STM (atomically)
import Data.Aeson
import Data.Bimap qualified as B
import Data.ByteString.Lazy qualified as Byte
import Data.Map.Strict qualified as M
import Foreign.Ptr
import Foreign.StablePtr
import System.Directory
import System.Posix.Types (Fd (..))
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Utils.KeyDispatches
import Wayland.Client

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
    river = getRiver
    xkbBindings = getXkbBindings
    layerShell = getLayerShell
    xkbConfig = getXkbConfig
    libinputConfig = getLibinputConfig
    inputManager = getInputManager

  exists <- doesFileExist (statePath myConfig)
  oldWindows <-
    if not exists
      then pure M.empty
      else do
        content <- Byte.readFile (statePath myConfig)
        case decode content of
          Just PersistedState{persistedWindows} -> do
            removeFile (statePath myConfig)
            pure persistedWindows
          _ -> pure M.empty

  fd <- createKeymapFd (composeKeyMap myConfig)
  -- q <- atomically $ newTQueue
  st <-
    newMVar
      WMState
        { manageQueue = pure ()
        , renderQueue = pure ()
        , allWindows = M.empty
        , focusedWindow = Nothing
        , allOutputs = M.empty
        , allLayerShellOutputs = M.empty
        , seatXkbBindings = M.empty
        , seatPointerBindings = M.empty
        , focusedOutput = nullPtr
        , allWorkspacesTiled = BS.empty
        , allWorkspacesFloating = BS.empty
        , allWorkspacesFullscreen = BS.empty
        , floatingQueue = M.fromList (zip [1 .. 9] (repeat []))
        , fullscreenQueue = M.fromList (zip [1 .. 9] (repeat []))
        , newWindowQueue = []
        , focusedSeat = nullPtr
        , allOutputWorkspaces = B.empty
        , lastFocusedWorkspace = 1
        , workspaceLayouts = defaultLayouts myConfig
        , currentWmManager = river
        , currentXkbBindings = xkbBindings
        , currentLayerShell = layerShell
        , currentXkbConfig = xkbConfig
        , opDeltaState = None
        , currentOpDelta = (0, 0, 0, 0)
        , cursorPosition = (0, 0)
        , persistedState = oldWindows
        , currentKeymapFd = fd
        , activeRepeater = Nothing
        -- , tQueue = q
        }
  stPtr <- newStablePtr st

  _ <- wlProxyAddListener (castPtr river) getRiverWmListener (castStablePtrToPtr stPtr)
  _ <- wlProxyAddListener (castPtr xkbConfig) getRiverXkbConfigListener (castStablePtrToPtr stPtr)
  _ <- wlProxyAddListener (castPtr libinputConfig) getRiverLibinputConfigListener (castStablePtrToPtr stPtr)
  _ <- wlProxyAddListener (castPtr inputManager) getRiverInputManagerListener (castStablePtrToPtr stPtr)

  _ <- wlDisplayRoundtrip display

  mapM_ (\str -> exec str nullPtr st) (execOnStart myConfig)

  -- wlFd <- wlDisplayGetFd display
  -- void $ forkIO $ forever $ do
  --   threadWaitRead (Fd wlFd)
  --   atomically $ writeTQueue q WlEvent
  --
  -- forever $ do
  --   event <- atomically $ readTQueue q
  --   case event of
  --     WlEvent -> do
  --       _ <- wlDisplayDispatch display
  --       pure ()
  --
  --     -- IPCCommand payload -> do
  --     --     -- Safely handle IPC without thread-affinity issues
  --     --     modifyMVar_ stateMVar $ \state -> do
  --     --         processIPC payload state
  --
  --     RepeatKey action -> do
  --       action
  forever $ do
    _ <- wlDisplayDispatch display
    pure ()
