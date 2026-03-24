{-# LANGUAGE MultiWayIf #-}

module Handlers.Window where

import Control.Concurrent.MVar
import Control.Monad.State hiding (state)
import Data.Bimap qualified as B
import Data.ByteString qualified as BStr
import Data.Map.Strict qualified as M
import Data.Maybe
import Data.Sequence qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Foreign
import Foreign.C
import Optics.State
import Optics.State.Operators
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
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
foreign export ccall "hs_window_fullscreen_requested"
  hsWindowFullscreenRequested :: Ptr () -> Ptr RiverWindow -> Ptr RiverOutput -> IO ()
foreign export ccall "hs_window_exit_fullscreen_requested"
  hsWindowExitFullscreenRequested :: Ptr () -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_window_maximize_requested"
  hsWindowMaximizeRequested :: Ptr () -> Ptr RiverWindow -> IO ()
foreign export ccall "hs_window_unmaximize_requested"
  hsWindowUnmaximizeRequested :: Ptr () -> Ptr RiverWindow -> IO ()

hsWindowIdentifier :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
hsWindowIdentifier dataPtr win identifier = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $
    \state@WMState
       { persistedState
       , allWindows
       , focusedOutput
       , fullscreenQueue
       , floatingQueue
       , allOutputWorkspaces
       , allWorkspacesTiled
       } -> do
        i <- peekCString identifier
        case M.lookup i persistedState of
          Nothing -> do
            let newWindows = M.adjust (\w -> w{winIdentifier = i}) win allWindows
            pure
              state
                { allWindows = newWindows
                , newWindowQueue = win : newWindowQueue state
                }
          Just (supposedWorkspace, windowStatus) -> do
            let newPersisted = M.delete i persistedState
                hidingAction =
                  if supposedWorkspace == fromMaybe 1 (B.lookup focusedOutput allOutputWorkspaces) then pure () else riverWindowHide win
                newState = case windowStatus of
                  Tiled -> do
                    let newWindows = M.adjust (\w -> w{winIdentifier = i}) win allWindows
                    state
                      { allWindows = newWindows
                      , allWorkspacesTiled = BS.insert supposedWorkspace win allWorkspacesTiled
                      }
                  Floating -> do
                    let newWindows = M.adjust (\w -> w{winIdentifier = i, isFloating = True}) win allWindows
                    state
                      { allWindows = newWindows
                      , floatingQueue = M.adjust (win :) supposedWorkspace floatingQueue
                      }
                  Fullscreen -> do
                    let newWindows = M.adjust (\w -> w{winIdentifier = i, isFullscreen = True}) win allWindows
                    state
                      { allWindows = newWindows
                      , fullscreenQueue = M.adjust (win :) supposedWorkspace fullscreenQueue
                      }
                  FullscreenFloating -> do
                    let newWindows = M.adjust (\w -> w{winIdentifier = i, isFullscreen = True, isFloating = True}) win allWindows
                    state
                      { allWindows = newWindows
                      , fullscreenQueue = M.adjust (win :) supposedWorkspace fullscreenQueue
                      }
            pure newState{persistedState = newPersisted, renderQueue = renderQueue state >> hidingAction}

hsWindowClosed :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowClosed dataPtr win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ \s -> do
    riverWindowDestroy win
    pure $ execState transform s
 where
  transform = do
    #allWindows %= M.delete win
    #allWorkspacesFloating %= BS.delete win
    #allWorkspacesTiled %= BS.delete win
    #allWorkspacesFullscreen %= BS.delete win
    fWinM <- use #focusedWindow
    fOutput <- use #focusedOutput
    outWorkmaps <- use #allOutputWorkspaces
    case (fWinM, B.lookup fOutput outWorkmaps) of
      (Just fWin, Just fWs) | fWin == win -> do
        seat <- use #focusedSeat
        use (workspaceWindows fWs) >>= \case
          S.Empty -> do
            #focusedWindow .= Nothing
            #workspaceFocusHistory %= M.delete fWs
            #manageQueue <>= riverSeatClearFocus seat
          h S.:<| _ -> do
            #focusedWindow ?= h
            #workspaceFocusHistory %= M.insert fWs h
            #manageQueue <>= riverSeatFocusWindow seat h
      _ -> pure ()

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
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let newWindows = M.adjust (\w -> w{parentWindow = Just parent}) win (allWindows state)
    pure state{allWindows = newWindows}

hsWindowDimensionsHint :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
hsWindowDimensionsHint dataPtr win minW minH maxW maxH = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $
    \state@WMState
       { allWindows
       , allOutputWorkspaces
       , focusedOutput
       , floatingQueue
       } -> do
        let newWindows = M.adjust (\w -> w{dimensionsHint = (minW, minH, maxW, maxH)}) win allWindows
            newState = state{allWindows = newWindows}
            focusedWorkspace = fromMaybe 1 (B.lookup focusedOutput allOutputWorkspaces)
        if minW == maxW && minH == maxH && minW /= 0 && minH /= 0 -- Window cannot change size, like dialog windows or splashes
          then
            pure
              newState
                { allWorkspacesTiled = BS.delete win (allWorkspacesTiled state)
                , allWorkspacesFullscreen = BS.delete win (allWorkspacesFullscreen state)
                , floatingQueue = M.adjust (win :) focusedWorkspace floatingQueue
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
       , allOutputWorkspaces
       , focusedOutput
       } -> do
        let window@Window{isFloating} = allWindows M.! win
            newWindows = M.insert win window{isFullscreen = True, isPinned = False} allWindows
            currentFocusedWorkspace = fromMaybe 1 $ B.lookup focusedOutput allOutputWorkspaces
            targetWorkspace = fromMaybe currentFocusedWorkspace (B.lookup output allOutputWorkspaces)
            newState
              | isFloating = state{allWorkspacesFloating = BS.delete win allWorkspacesFloating}
              | otherwise = state{allWorkspacesTiled = BS.delete win allWorkspacesTiled}

        pure
          newState
            { allWindows = newWindows
            , fullscreenQueue = M.adjust (win :) targetWorkspace fullscreenQueue
            , allWorkspacesFullscreen = BS.insert targetWorkspace win allWorkspacesFullscreen
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
       , manageQueue
       , allOutputWorkspaces
       } -> do
        let Window{isFloating} = allWindows M.! win
            workspace = fromMaybe (fromMaybe 1 $ B.lookup focusedOutput allOutputWorkspaces) (BS.lookupA win allWorkspacesFullscreen)
            newState
              | isFloating = state{allWorkspacesFloating = BS.insert workspace win allWorkspacesFloating}
              | otherwise = state{allWorkspacesTiled = BS.insert workspace win allWorkspacesTiled}
        pure
          newState
            { allWindows = M.adjust (\w -> w{isFullscreen = False}) win allWindows
            , manageQueue = manageQueue >> riverWindowExitFullscreen win >> riverWindowInformNotFullscreen win
            , allWorkspacesFullscreen = BS.delete win allWorkspacesFullscreen
            }

hsWindowMaximizeRequested :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowMaximizeRequested dataPtr win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let newWindows = M.adjust (\w -> w{isMaximized = True}) win (allWindows state)
    pure state{allWindows = newWindows, manageQueue = manageQueue state >> riverWindowInformMaximized win}

hsWindowUnmaximizeRequested :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowUnmaximizeRequested dataPtr win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ stateMVar $ \state -> do
    let newWindows = M.adjust (\w -> w{isMaximized = False}) win (allWindows state)
    pure state{allWindows = newWindows, manageQueue = manageQueue state >> riverWindowInformUnmaximized win}
