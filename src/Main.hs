module Main where

import Config
import Control.Concurrent
import Control.Concurrent.STM.TQueue
import Control.Monad (forever)
import Control.Monad.STM (atomically)
import Data.Aeson
import Data.Bimap qualified as B
import Data.ByteString.Lazy qualified as Byte
import Data.Map.Strict qualified as M
import Foreign
import Handlers.Registry
import IPC
import System.Directory
import System.IO
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

  exists <- doesFileExist (statePath myConfig)
  (oldWindows, oldOutputs) <-
    if not exists
      then pure (M.empty, M.empty)
      else do
        content <- Byte.readFile (statePath myConfig)
        case decode content of
          Just PersistedState{persistedWindows, persistedOutputs} -> do
            removeFile (statePath myConfig)
            pure (persistedWindows, persistedOutputs)
          _ -> pure (M.empty, M.empty)

  fd <- createKeymapFd (composeKeyMap myConfig)
  queue <- atomically $ newTQueue
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
        , allWorkspacesTiled = BS.empty
        , allWorkspacesFloating = BS.empty
        , allWorkspacesFullscreen = BS.empty
        , floatingQueue = M.fromList (zip [1 .. 9] (repeat []))
        , fullscreenQueue = M.fromList (zip [1 .. 9] (repeat []))
        , newWindowQueue = []
        , focusedSeat = nullPtr
        , allSeats = M.empty
        , allWlSeats = M.empty
        , allOutputWorkspaces = B.empty
        , lastFocusedWorkspace = 1
        , workspaceLayouts = defaultLayouts myConfig
        , currentWindowManager = nullPtr
        , currentXkbBindings = nullPtr
        , currentLayerShell = nullPtr
        , currentXkbConfig = nullPtr
        , currentCursorShapeManager = nullPtr
        , opDeltaState = None
        , currentOpDelta = (0, 0, 0, 0)
        , cursorPosition = (0, 0)
        , persistedStateWindows = oldWindows
        , persistedStateOutputs = oldOutputs
        , workspaceFocusHistory = M.empty
        , currentKeymapFd = fd
        , subscribers = []
        }
  stPtr <- newStablePtr st

  reg <- makeRegistryGlobalCallback registryGlobal
  regRemove <- makeRegistryGlobalRemoveCallback registryGlobalRemove
  listenerPtr <- malloc :: IO (Ptr WlRegistryListener)
  poke listenerPtr (WlRegistryListener reg regRemove)
  _ <- wlProxyAddListener (castPtr registry) (castPtr listenerPtr) (castStablePtrToPtr stPtr)

  _ <- wlDisplayRoundtrip display

  mapM_ (\str -> exec str nullPtr st) (execOnStart myConfig)

  startIPCListener "/tmp/rivermonad.sock" queue

  forever $ do
    event <- atomically $ tryReadTQueue queue
    case event of
      Just (IPCEvent s conn) -> do
        case s of
          "Subscribe" -> do
            modifyMVar_ st $ \state -> return state{subscribers = conn : subscribers state}
          _ -> pure ()
      Nothing -> do
        _ <- wlDisplayDispatch display
        hFlush stdout
