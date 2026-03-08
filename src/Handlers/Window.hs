{-# LANGUAGE MultiWayIf #-}

module Handlers.Window where

import Control.Concurrent.MVar
import Data.Bimap qualified as B
import Data.ByteString qualified as BStr
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Foreign
import Foreign.C
import System.IO
import Types
import Utils.BiSeqMap qualified as BS
import Wayland.ImportedFunctions

foreign export ccall "hs_window_closed"
  hsWindowClosed :: Ptr () -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_window_dimensions"
  hsWindowDimensions :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> IO ()
foreign export ccall "hs_window_parent"
  hsWindowParent :: Ptr () -> Ptr RiverWindow -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_window_dimensions_hint"
  hsWindowDimensionsHint :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
foreign export ccall "hs_window_title"
  hsWindowTitle :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
foreign export ccall "hs_window_app_id"
  hsWindowAppID :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
foreign export ccall "hs_window_identifier"
  hsWindowIdentifier :: Ptr () -> Ptr RiverWindow -> CString -> IO ()

hsWindowIdentifier :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
hsWindowIdentifier dataPtr win identifier = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state@WMState{persistedState, allWindows} -> do
    i <- peekCString identifier
    print i
    case M.lookup i persistedState of
      Nothing -> do
        let newWindows = M.adjust (\w -> w{winIdentifier = i}) win allWindows
        pure
          state
            { allWindows = newWindows
            , newWindowQueue = win : newWindowQueue state
            -- , focusedWindow = Just (win)
            }
      Just (supposedWorkspace, windowStatus) -> do
        let newPersisted = M.delete i persistedState
            hidingAction =
              if supposedWorkspace == fromMaybe 1 (B.lookup (focusedOutput state) (allOutputWorkspaces state)) then pure () else riverWindowHide win
            s = case windowStatus of
              Tiled -> do
                let newWindows = M.adjust (\w -> w{winIdentifier = i}) win allWindows
                    newWorkspacesTiled = BS.insert supposedWorkspace win (allWorkspacesTiled state)
                state
                  { allWindows = newWindows
                  , allWorkspacesTiled = newWorkspacesTiled
                  }
              Floating -> do
                let newWindows = M.adjust (\w -> w{winIdentifier = i, isFloating = True}) win allWindows
                state
                  { allWindows = newWindows
                  , floatingQueue = M.adjust (win :) supposedWorkspace (floatingQueue state)
                  }
              Fullscreen -> do
                let newWindows = M.adjust (\w -> w{winIdentifier = i, isFullscreen = True}) win allWindows
                state
                  { allWindows = newWindows
                  , fullscreenQueue = M.adjust (win :) supposedWorkspace (fullscreenQueue state)
                  }
              FullscreenFloating -> do
                let newWindows = M.adjust (\w -> w{winIdentifier = i, isFullscreen = True, isFloating = True}) win allWindows
                state
                  { allWindows = newWindows
                  , fullscreenQueue = M.adjust (win :) supposedWorkspace (fullscreenQueue state)
                  }
        pure s{persistedState = newPersisted, renderQueue = renderQueue state >> hidingAction}

hsWindowClosed :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowClosed dataPtr win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let newWindows = M.delete win (allWindows state)
        newWorkspacesTiled = BS.delete win (allWorkspacesTiled state)
        newWorkspacesFloating = BS.delete win (allWorkspacesFloating state)
        newWorkspacesFullscreen = BS.delete win (allWorkspacesFullscreen state)
        f = focusedWindow state
        newFocusedWin
          | isNothing f = Nothing
          | fromJust f /= win = f
          | otherwise = Nothing
    riverWindowDestroy win
    pure
      state
        { allWindows = newWindows
        , allWorkspacesTiled = newWorkspacesTiled
        , allWorkspacesFullscreen = newWorkspacesFullscreen
        , allWorkspacesFloating = newWorkspacesFloating
        , focusedWindow = newFocusedWin
        }

hsWindowDimensions :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> IO ()
hsWindowDimensions dataPtr winP width height = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state@WMState{opDeltaState} -> do
    case opDeltaState of
      None -> do
        let w@Window{isFloating, floatingGeometry, isFullscreen} = allWindows state M.! winP
        if
          | isFullscreen -> pure state
          | isFloating ->
              do
                let newGeometry =
                      (fromMaybe (Rect 0 0 0 0) floatingGeometry)
                        { rw = width
                        , rh = height
                        }
                    newWindow = w{floatingGeometry = Just newGeometry}
                    newAllWindows = M.insert winP newWindow (allWindows state)
                pure state{allWindows = newAllWindows}
          | otherwise -> pure state
      _ -> pure state

hsWindowParent :: Ptr () -> Ptr RiverWindow -> Ptr RiverWindow -> IO ()
hsWindowParent dataPtr win parent = do
  print "Has Parent"
  hFlush stdout
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let newWindows = M.adjust (\w -> w{parentWindow = Just parent}) win (allWindows state)
    pure state{allWindows = newWindows}

hsWindowDimensionsHint :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
hsWindowDimensionsHint dataPtr win minW minH maxW maxH = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state@WMState{allWindows} -> do
    let newWindows = M.adjust (\w -> w{dimensionsHint = (minW, minH, maxW, maxH)}) win allWindows
        newState = state{allWindows = newWindows}
        focusedWorkspace = fromMaybe 1 (B.lookup (focusedOutput state) (allOutputWorkspaces state))
    if minW == maxW && minH == maxH && minW /= 0 && minH /= 0
      then do
        let newTiled = BS.delete win (allWorkspacesTiled state)
            newFullscreen = BS.delete win (allWorkspacesFullscreen state)
        pure
          newState
            { allWorkspacesTiled = newTiled
            , allWorkspacesFullscreen = newFullscreen
            , floatingQueue = M.adjust (win :) focusedWorkspace (floatingQueue state)
            }
      else pure newState

hsWindowTitle :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
hsWindowTitle dataPtr win title = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state ->
    if title == nullPtr
      then pure state
      else do
        bs <- BStr.packCString title
        let decoded = T.unpack $ TE.decodeUtf8With TEE.lenientDecode bs
            newWindows = M.adjust (\w -> w{winTitle = decoded}) win (allWindows state)
        print decoded
        hFlush stdout
        pure state{allWindows = newWindows}

hsWindowAppID :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
hsWindowAppID dataPtr win appID = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state ->
    if appID == nullPtr
      then pure state
      else do
        bs <- BStr.packCString appID
        let decoded = T.unpack $ TE.decodeUtf8With TEE.lenientDecode bs
            newWindows = M.adjust (\w -> w{winAppID = decoded}) win (allWindows state)
        print decoded
        hFlush stdout
        pure state{allWindows = newWindows}

hsWindowFullscreenRequested :: Ptr () -> Ptr RiverWindow -> Ptr RiverOutput -> IO ()
hsWindowFullscreenRequested dataPtr win output = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $
    \state@WMState
       { allWindows
       , allWorkspacesFloating
       , allWorkspacesTiled
       , allWorkspacesFullscreen
       , fullscreenQueue
       } -> do
        let window@Window{isFloating} = allWindows M.! win
            newWindows = M.insert win window{isFullscreen = True} allWindows
            newState
              | isFloating = state{allWorkspacesFloating = BS.delete win allWorkspacesFloating}
              | otherwise = state{allWorkspacesTiled = BS.delete win allWorkspacesTiled}
            focusedWorkspace = fromMaybe 1 (B.lookup output $ allOutputWorkspaces state)

        pure
          newState
            { allWindows = newWindows
            , fullscreenQueue = M.adjust (win :) focusedWorkspace fullscreenQueue
            , allWorkspacesFullscreen = BS.insert focusedWorkspace win allWorkspacesFullscreen
            }

hsWindowExitFullscreenRequested :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowExitFullscreenRequested dataPtr win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $
    \state@WMState
       { allWindows
       , allWorkspacesFloating
       , allWorkspacesTiled
       , allWorkspacesFullscreen
       , focusedOutput
       , allOutputWorkspaces
       } -> do
        let window@Window{isFloating} = allWindows M.! win
            newWindows = M.insert win window{isFullscreen = False} allWindows
            workspace = fromMaybe (allOutputWorkspaces B.! focusedOutput) (BS.lookupA win allWorkspacesFullscreen)
            newState
              | isFloating = state{allWorkspacesFloating = BS.insert workspace win allWorkspacesFloating}
              | otherwise = state{allWorkspacesTiled = BS.insert workspace win allWorkspacesTiled}
        pure
          newState
            { allWindows = newWindows
            , manageQueue = manageQueue state >> riverWindowExitFullscreen win
            , allWorkspacesFullscreen = BS.delete win allWorkspacesFullscreen
            }
