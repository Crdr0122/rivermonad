module Main where

import Config
import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM.TQueue
import Control.Monad (forever)
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.STM (atomically)
import Data.Aeson hiding (Object)
import Data.Bimap qualified as B
import Data.ByteString.Lazy qualified as Byte
import Data.Map.Strict qualified as M
import Handlers.Registry
import IPC
import System.Directory
import System.Posix.Types (Fd (..))
import Types
import Utils.BiSeqMap qualified as BS
import Utils.KeyDispatches
import Utils.Keymap
import Wayland.Connection
import Wayland.Generated

main :: IO ()
main = do
  (oldWindows, oldOutputs) <-
    doesFileExist (statePath myConfig) >>= \case
      False -> pure (M.empty, M.empty)
      True ->
        decode <$> (Byte.readFile (statePath myConfig)) >>= \case
          Just PersistedState{persistedWindows, persistedOutputs} -> do
            removeFile (statePath myConfig)
            pure (persistedWindows, persistedOutputs)
          _ -> pure (M.empty, M.empty)
  fd <- rmlvoToKeymapFd (keyboardOptions myConfig)
  queue <- atomically $ newTQueue

  mvar <-
    newMVar
      WMState
        { manageQueue = pure ()
        , renderQueue = pure ()
        , allWindows = M.empty
        , focusedWin = Nothing
        , allOutputs = M.empty
        , allLayerShellOutputs = M.empty
        , focusedOut = nonObject
        , allWorkspacesTiled = BS.empty
        , allWorkspacesFloating = BS.empty
        , allWorkspacesFullscreen = BS.empty
        , floatingQueue = M.fromList (zip [1 .. 9] (repeat []))
        , fullscreenQueue = M.fromList (zip [1 .. 9] (repeat []))
        , newWindowQueue = []
        , focusedSeat = nonObject
        , allSeats = M.empty
        , allWlSeats = M.empty
        , allOutputWorkspaces = B.empty
        , lastFocusedWorkspace = 1
        , workspaceLayouts = defaultLayouts myConfig
        , currentWM = nonObject
        , currentXkbBindings = nonObject
        , currentLayerShell = nonObject
        , currentXkbConfig = nonObject
        , currentCursorShapeManager = nonObject
        , opDeltaState = None
        , currentOpDelta = (0, 0, 0, 0)
        , cursorPosition = (0, 0)
        , persistedStateWindows = oldWindows
        , persistedStateOutputs = oldOutputs
        , workspaceFocusHistory = M.empty
        , currentKeymapFd = Fd <$> fd
        , subscribers = []
        }

  startIPCListener "/tmp/rivermonad.sock" queue

  _ <- forkIO $ forever $ do
    (IPCEvent s conn) <- atomically $ readTQueue queue
    case s of
      "Subscribe" -> do
        modifyMVar_ mvar $ \state -> return state{subscribers = conn : subscribers state}
      _ -> pure ()

  let displayHandlers =
        WlDisplayHandlers
          { onWlDisplayError = \_ obj code msg -> liftIO $ putStrLn ("FATAL wl_display.error: object=" <> show obj <> " code=" <> show code <> " msg=" <> show msg)
          , onWlDisplayDeleteId = \_ _ -> pure ()
          }
      registryHandlers = mkRegistryHandlers mvar

  (disp, aThread) <- connect displayHandlers registryHandlers


  -- This needs to be after the ipc listener, or else it might connect to an earlier wm and freeze everything
  mapM_ (\cmd -> runReaderT (exec cmd nonObject mvar) (displayEnv disp)) (execOnStart myConfig)

  wait aThread
