module IPC (startIPCListener, broadcastState) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (SomeException, catch, throwIO)
import Control.Monad
import Data.ByteString.Char8 qualified as BC
import Network.Socket
import Network.Socket.ByteString (recv, sendAll)
import System.Directory (removeFile)
import System.IO.Error (isDoesNotExistError)
import Types

removeIfExists :: FilePath -> IO ()
removeIfExists fileName = removeFile fileName `catch` handleExists
 where
  handleExists e
    | isDoesNotExistError e = return ()
    | otherwise = throwIO e

startIPCListener :: FilePath -> TQueue WMEvent -> IO ()
startIPCListener path q = do
  _ <- removeIfExists path
  sock <- socket AF_UNIX Stream 0
  bind sock (SockAddrUnix path)
  listen sock 5

  void $ forkIO $ forever $ do
    (conn, _) <- accept sock
    void $ forkIO $ do
      -- Read the command (up to 1024 bytes)
      msg <- recv conn 1024
      let cmd = BC.unpack msg
      print cmd

      -- Push to main loop
      atomically $ writeTQueue q $ IPCEvent cmd conn

trySend :: BC.ByteString -> Socket -> IO Bool
trySend msg sock =
  (sendAll sock msg >> return True) `catch` \(_ :: SomeException) -> do
    -- If we hit an error, the pipe is broken.
    -- Close our end to free the File Descriptor.
    close sock
    return False

broadcastState :: WMState -> String -> IO WMState
broadcastState state msg = do
  let payload = BC.pack (msg ++ "\n")
  -- filterM runs the IO action (trySend) for each socket
  activeSubscribers <- filterM (trySend payload) (subscribers state)
  return state{subscribers = activeSubscribers}
