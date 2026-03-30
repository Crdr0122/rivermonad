module Handlers.Window where

import Control.Concurrent.MVar
import Control.Monad (msum, unless, when)
import Control.Monad.State hiding (state)
import Data.Bimap qualified as B
import Data.ByteString qualified as BStr
import Data.Map.Strict qualified as M
import Data.Maybe (fromMaybe)
import Data.Sequence qualified as S
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Foreign
import Foreign.C
import Optics.Core
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
hsWindowIdentifier dataPtr win identifierPtr = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  ident <- peekCString identifierPtr
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState (transform ident)
 where
  transform ident = do
    #allWindows % at win %? #winIdentifier .= ident
    use (#persistedStateWindows % at ident) >>= \case
      Nothing -> #newWindowQueue %= (win :)
      Just (ws, status) -> do
        #persistedStateWindows % at ident .= Nothing
        fWs <- use focusedWorkspace
        #renderQueue <>= unless (ws == fromMaybe 1 fWs) (riverWindowHide win)
        case status of
          Tiled -> do
            #allWorkspacesTiled %= BS.insert ws win
          Floating -> do
            #floatingQueue % at ws %?= (win :)
            #allWindows % at win %? #isFloating .= True
          Fullscreen -> do
            #floatingQueue % at ws %?= (win :)
            #allWindows % at win %? #isFullscreen .= True
          FullscreenFloating -> do
            #fullscreenQueue % at ws %?= (win :)
            #allWindows % at win %? #isFloating .= True
            #allWindows % at win %? #isFullscreen .= True

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
    #workspaceFocusHistory %= M.filter (/= win)
    use (pairOfGetter #focusedWindow focusedWorkspace) >>= \case
      (Just fWin, Just ws) | fWin == win -> do
        seat <- use #focusedSeat
        use (workspaceWindows ws) >>= \case
          S.Empty -> do
            #focusedWindow .= Nothing
            #workspaceFocusHistory %= M.delete ws
            #manageQueue <>= riverSeatClearFocus seat
          h S.:<| _ -> do
            #focusedWindow ?= h
            #workspaceFocusHistory %= M.insert ws h
            #manageQueue <>= riverSeatFocusWindow seat h
      _ -> pure ()

hsWindowDimensions :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> IO ()
hsWindowDimensions dataPtr winPtr w h = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState updateDimensions
 where
  updateDimensions =
    use (pairOfGetter #opDeltaState (#allWindows % at winPtr)) >>= \case
      (None, Just winRec) -> do
        let isFloat = view #isFloating winRec
            isFull = view #isFullscreen winRec
        when (isFloat && not isFull) $ #allWindows % at winPtr %? #floatingGeometry %?= \r -> r{rw = w, rh = h}
      _ -> pure ()

hsWindowParent :: Ptr () -> Ptr RiverWindow -> Ptr RiverWindow -> IO ()
hsWindowParent dataPtr win parent = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . (#allWindows % at win %? #parentWindow ?~ parent)

hsWindowDimensionsHint :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
hsWindowDimensionsHint dataPtr win minW minH maxW maxH = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
 where
  transform = do
    #allWindows % at win %? #dimensionsHint .= (minW, minH, maxW, maxH)
    when (minW == maxW && minH == maxH && minW /= 0 && minH /= 0) $ do
      #allWorkspacesTiled %= BS.delete win
      #allWorkspacesFloating %= BS.delete win
      #allWorkspacesFullscreen %= BS.delete win
      use focusedWorkspace >>= \case
        Just focusedWs -> #floatingQueue % at focusedWs %?= (win :)
        Nothing -> pure ()

hsWindowTitle :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
hsWindowTitle dataPtr win title = do
  unless (title == nullPtr) $ do
    stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
    modifyMVar_ (stateMVar :: MVar WMState) $ \state -> do
      bs <- BStr.packCString title
      let decoded = T.unpack $ TE.decodeUtf8With TEE.lenientDecode bs
      pure $ state & #allWindows % at win %? #winTitle .~ decoded

hsWindowAppID :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
hsWindowAppID dataPtr win appID = do
  unless (appID == nullPtr) $ do
    stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
    modifyMVar_ (stateMVar :: MVar WMState) $ \state -> do
      bs <- BStr.packCString appID
      let decoded = T.unpack $ TE.decodeUtf8With TEE.lenientDecode bs
      pure $ state & #allWindows % at win %? #winAppID .~ decoded

hsWindowFullscreenRequested :: Ptr () -> Ptr RiverWindow -> Ptr RiverOutput -> IO ()
hsWindowFullscreenRequested dataPtr win output = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
 where
  transform = do
    focusedWs <- use focusedWorkspace
    targetWs <- use (#allOutputWorkspaces % to (B.lookup output))
    let actualWs = fromMaybe 1 $ msum [targetWs, focusedWs]
    #allWindows % at win %?= (\w -> w{isFullscreen = True, isPinned = False})
    #allWorkspacesFloating %= BS.delete win
    #allWorkspacesTiled %= BS.delete win
    #allWorkspacesFullscreen %= BS.delete win
    #fullscreenQueue % at actualWs %?= (win :)

hsWindowExitFullscreenRequested :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowExitFullscreenRequested dataPtr win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
 where
  transform =
    use (pairOfGetter (#allWindows % at win) (#allWorkspacesFullscreen % to (BS.lookupA win))) >>= \case
      (Just Window{isFloating}, Just ws) -> do
        #allWindows % at win %? #isFullscreen .= False
        #allWorkspacesFullscreen %= BS.delete win
        #manageQueue <>= (riverWindowExitFullscreen win >> riverWindowInformNotFullscreen win)
        if isFloating
          then #floatingQueue % at ws %?= (win :)
          else #allWorkspacesTiled %= BS.insert ws win
      _ -> pure ()

hsWindowMaximizeRequested :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowMaximizeRequested dataPtr win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $
    pure . (#manageQueue <>~ riverWindowInformMaximized win) . (#allWindows % at win %? #isMaximized .~ True)

hsWindowUnmaximizeRequested :: Ptr () -> Ptr RiverWindow -> IO ()
hsWindowUnmaximizeRequested dataPtr win = do
  stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
  modifyMVar_ (stateMVar :: MVar WMState) $
    pure . (#manageQueue <>~ riverWindowInformUnmaximized win) . (#allWindows % at win %? #isMaximized .~ False)
