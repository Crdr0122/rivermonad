module Handlers.Window (mkWindowHandler) where

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
import Protocols.Generated
import Types
import Utils.BiSeqMap qualified as BS
import Utils.Helpers
import Wayland.Connection

mkWindowHandler :: MVar WMState -> RiverWindowV1Handlers
mkWindowHandler mvar =
  RiverWindowV1Handlers
    { onRiverWindowV1CaptureSessions = \_ _ -> pure ()
    , onRiverWindowV1Closed = \_ -> pure ()
    , onRiverWindowV1AppId = \_ _ -> pure ()
    , onRiverWindowV1Dimensions = \_ _ _ -> pure ()
    , onRiverWindowV1DimensionsHint = \_ _ _ _ _ -> pure ()
    , onRiverWindowV1DecorationHint = \_ _ -> pure ()
    , onRiverWindowV1Title = \_ _ -> pure ()
    , onRiverWindowV1FullscreenRequested = \_ _ -> pure ()
    , onRiverWindowV1ExitFullscreenRequested = \_ -> pure ()
    , onRiverWindowV1Parent = \_ _ -> pure ()
    , onRiverWindowV1PointerMoveRequested = \_ _ -> pure ()
    , onRiverWindowV1PointerResizeRequested = \_ _ _ -> pure ()
    , onRiverWindowV1Identifier = \_ _ -> pure ()
    , onRiverWindowV1MaximizeRequested = \_ -> pure ()
    , onRiverWindowV1UnmaximizeRequested = \_ -> pure ()
    , onRiverWindowV1MinimizeRequested = \_ -> pure ()
    , onRiverWindowV1ShowWindowMenuRequested = \_ _ _ -> pure ()
    , onRiverWindowV1PresentationHint = \_ _ -> pure ()
    , onRiverWindowV1UnreliablePid = \_ _ -> pure ()
    }

-- hsWindowIdentifier :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
-- hsWindowIdentifier dataPtr win identifierPtr = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   ident <- peekCString identifierPtr
--   modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState (transform ident)
--  where
--   transform ident = do
--     #allWindows % at win %? #winIdentifier .= ident
--     use (#persistedStateWindows % at ident) >>= \case
--       Nothing -> #newWindowQueue %= (win :)
--       Just (ws, status) -> do
--         #persistedStateWindows % at ident .= Nothing
--         fWs <- use (focusedWorkspace % non 1)
--         unless (ws == fWs) $ #renderQueue <>= riverWindowHide win
--         case status of
--           Tiled -> do
--             #allWorkspacesTiled %= BS.insert ws win
--           Floating -> do
--             #floatingQueue % at ws %?= (win :)
--             #allWindows % at win %? #isFloating .= True
--           Fullscreen -> do
--             #fullscreenQueue % at ws %?= (win :)
--             #allWindows % at win %? #isFullscreen .= True
--           FullscreenFloating -> do
--             #fullscreenQueue % at ws %?= (win :)
--             #allWindows % at win %? #isFloating .= True
--             #allWindows % at win %? #isFullscreen .= True
--
closed :: MVar WMState -> Object RiverWindowV1 -> W ()
closed mvar win = do
  modifyMVarW_ mvar $ \s -> do
    riverWindowV1Destroy win
    pure $ execState transform s
 where
  transform = do
    #allWindows %= M.delete win
    #workspaceFocusHistory %= M.filter (/= win)
    deleteWinObjs win
    use (pairOfGetter #focusedWin focusedWorkspace) >>= \case
      (Just fWin, Just ws) | fWin == win -> do
        use (workspaceWindows ws) >>= \case
          S.Empty -> do
            #focusedWin .= Nothing
            #workspaceFocusHistory %= M.delete ws
          h S.:<| _ -> do
            setFocusedWindowAndHistory ws h
      _ -> pure ()

dimensions :: MVar WMState -> Object RiverWindowV1 -> Int32 -> Int32 -> W ()
dimensions mvar winPtr w h = do
  modifyMVarW_ mvar $ pure . execState updateDimensions
 where
  updateDimensions =
    use (pairOfGetter #opDeltaState (#allWindows % at winPtr)) >>= \case
      (None, Just winRec) -> do
        let isFloat = view #winFloat winRec
            isFull = view #winFull winRec
        when (isFloat && not isFull) $ #allWindows % at winPtr %? #winFloatGeo %?= \r -> r{rw = w, rh = h}
      _ -> pure ()

winParent :: MVar WMState -> Object RiverWindowV1 -> Object RiverWindowV1 -> W ()
winParent mvar win parent = do
  modifyMVarW_ mvar $ pure . execState transform
 where
  transform = do
    #allWindows % at win %? #winParent ?= parent
    deleteWinObjs win
    use focusedWorkspace >>= \case
      Just focusedWs -> #floatingQueue % at focusedWs %?= (win :)
      Nothing -> pure ()

--
-- hsWindowDimensionsHint :: Ptr () -> Ptr RiverWindow -> CInt -> CInt -> CInt -> CInt -> IO ()
-- hsWindowDimensionsHint dataPtr win minW minH maxW maxH = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
--  where
--   transform = do
--     #allWindows % at win %? #dimensionsHint .= (minW, minH, maxW, maxH)
--     when (minW == maxW && minH == maxH && minW /= 0 && minH /= 0) $ do
--       deleteWinPtrs win
--       use focusedWorkspace >>= \case
--         Just focusedWs -> #floatingQueue % at focusedWs %?= (win :)
--         Nothing -> pure ()
--
winTitle :: Ptr () -> Ptr RiverWindowV1 -> CString -> IO ()
winTitle dataPtr win title = do
  unless (title == nullPtr) $ do
    stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
    modifyMVar_ (stateMVar :: MVar WMState) $ \state -> do
      bs <- BStr.packCString title
      let decoded = T.unpack $ TE.decodeUtf8With TEE.lenientDecode bs
      pure $ state & #allWindows % at win %? #winTitle .~ decoded
--
-- hsWindowAppID :: Ptr () -> Ptr RiverWindow -> CString -> IO ()
-- hsWindowAppID dataPtr win appID = do
--   unless (appID == nullPtr) $ do
--     stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--     modifyMVar_ (stateMVar :: MVar WMState) $ \state -> do
--       bs <- BStr.packCString appID
--       let decoded = T.unpack $ TE.decodeUtf8With TEE.lenientDecode bs
--       pure $ state & #allWindows % at win %? #winAppID .~ decoded
--
-- hsWindowFullscreenRequested :: Ptr () -> Ptr RiverWindow -> Ptr RiverOutput -> IO ()
-- hsWindowFullscreenRequested dataPtr win output = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
--  where
--   transform = do
--     focusedWs <- use focusedWorkspace
--     targetWs <- use (#allOutputWorkspaces % to (B.lookup output))
--     let actualWs = fromMaybe 1 $ msum [targetWs, focusedWs]
--     #allWindows % at win %?= (\w -> w{isFullscreen = True, isPinned = False})
--     deleteWinPtrs win
--     #fullscreenQueue % at actualWs %?= (win :)
--
-- hsWindowExitFullscreenRequested :: Ptr () -> Ptr RiverWindow -> IO ()
-- hsWindowExitFullscreenRequested dataPtr win = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ (stateMVar :: MVar WMState) $ pure . execState transform
--  where
--   transform =
--     use (pairOfGetter (#allWindows % at win) (#allWorkspacesFullscreen % to (BS.lookupA win))) >>= \case
--       (Just Window{isFloating}, Just ws) -> do
--         #allWindows % at win %? #isFullscreen .= False
--         #allWorkspacesFullscreen %= BS.delete win
--         #manageQueue <>= (riverWindowExitFullscreen win >> riverWindowInformNotFullscreen win)
--         if isFloating
--           then #floatingQueue % at ws %?= (win :)
--           else #allWorkspacesTiled %= BS.insert ws win
--       _ -> pure ()
--
-- hsWindowMaximizeRequested :: Ptr () -> Ptr RiverWindow -> IO ()
-- hsWindowMaximizeRequested dataPtr win = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ (stateMVar :: MVar WMState) $
--     pure . (#manageQueue <>~ riverWindowInformMaximized win) . (#allWindows % at win %? #isMaximized .~ True)
--
-- hsWindowUnmaximizeRequested :: Ptr () -> Ptr RiverWindow -> IO ()
-- hsWindowUnmaximizeRequested dataPtr win = do
--   stateMVar <- deRefStablePtr (castPtrToStablePtr dataPtr)
--   modifyMVar_ (stateMVar :: MVar WMState) $
--     pure . (#manageQueue <>~ riverWindowInformUnmaximized win) . (#allWindows % at win %? #isMaximized .~ False)
