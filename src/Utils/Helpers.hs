module Utils.Helpers (
  calculateFloatingPosition,
  calculateFloatingPositions,
  workspaceWindows,
  focusedWorkspace,
  setFocusedWindowAndHistory,
  focusedOutputGeom,
  createKeymapFd,
  pairOfGetter,
  pairOf,
) where

import Control.Monad.State
import Data.Bimap as B
import Data.Maybe
import Data.Sequence as S
import Foreign
import Foreign.C
import Optics.Core
import Optics.State.Operators
import System.IO
import System.Posix.IO
import System.Posix.Types (Fd (..))
import Types
import Utils.BiSeqMap qualified as BS
import Wayland.ImportedFunctions

setFocusedWindowAndHistory :: (MonadState WMState m) => WorkspaceID -> Ptr RiverWindow -> m ()
setFocusedWindowAndHistory ws w = do
  #focusedWindow ?= w
  #workspaceFocusHistory % at ws ?= w

calculateFloatingPositions :: (Functor f, Foldable f) => Rect -> f Window -> ([Rect], IO (), IO ())
calculateFloatingPositions o windows = res
 where
  resInter = fmap (\win -> calculateFloatingPosition (winPtr win) win o) windows
  res =
    foldl'
      (\(rects, ms, rs) (rect, m, r) -> (rect : rects, ms >> m, rs >> r))
      ([], pure (), pure ())
      resInter

calculateFloatingPosition :: Ptr RiverWindow -> Window -> Rect -> (Rect, IO (), IO ())
calculateFloatingPosition
  win
  Window{floatingGeometry, nodePtr, dimensionsHint, parentWindow}
  Rect{rh = outHeight, rw = outWidth, rx = outX, ry = outY} = case floatingGeometry of
    Nothing -> case dimensionsHint of
      (0, 0, _, _) ->
        ( Rect{rx = offsetX, ry = offsetY, rw = w, rh = h}
        , riverWindowProposeDimensions win w h
        , riverNodeSetPosition nodePtr (offsetX + outX) (offsetY + outY)
        )
      (minW, minH, _, _)
        | isJust parentWindow ->
            let minY = (outHeight - minH) `div` 2
                minX = (outWidth - minW) `div` 2
             in ( Rect{rx = minX, ry = minY, rw = minW, rh = minH}
                , riverWindowProposeDimensions win minW minH
                , riverNodeSetPosition nodePtr (minX + outX) (minY + outY)
                )
      (_, _, _, _) ->
        ( Rect{rx = offsetX, ry = offsetY, rw = w, rh = h}
        , riverWindowProposeDimensions win w h
        , riverNodeSetPosition nodePtr (offsetX + outX) (offsetY + outY)
        )
    Just g@Rect{rx, ry, rw, rh} ->
      ( g
      , riverWindowProposeDimensions win rw rh
      , riverNodeSetPosition nodePtr (outX + rx) (outY + ry)
      )
   where
    w = outWidth * 6 `div` 10
    h = outHeight * 6 `div` 10
    offsetX = (outWidth - w) `div` 2
    offsetY = (outHeight - h) `div` 2

workspaceWindows :: WorkspaceID -> Getter WMState (Seq (Ptr RiverWindow))
workspaceWindows ws = to $ \s ->
  (s ^. #allWorkspacesFullscreen % to (BS.lookupBs ws))
    S.>< (s ^. #allWorkspacesTiled % to (BS.lookupBs ws))
    S.>< (s ^. #allWorkspacesFloating % to (BS.lookupBs ws))

focusedWorkspace :: Getter WMState (Maybe WorkspaceID)
focusedWorkspace = to $ \s -> s ^? #allOutputWorkspaces % to (B.lookup (s ^. #focusedOutput)) % _Just

focusedOutputGeom :: Getter WMState (Maybe Rect)
focusedOutputGeom = to $ \s -> s ^? #allOutputs % at (s ^. #focusedOutput) %? #outGeometry

pairOf :: Lens' s a -> Lens' s b -> Lens' s (a, b)
pairOf la lb = lens getter setter
 where
  getter s = (s ^. la, s ^. lb)
  setter s (x, y) = s & la .~ x & lb .~ y

pairOfGetter :: (Is k A_Getter, Is l A_Getter) => Optic' k is s a -> Optic' l js s b -> Getter s (a, b)
pairOfGetter ga gb = to $ \s -> (s ^. ga, s ^. gb)

-- You'll need to import these from a library like 'unix' or bind them via FFI
foreign import ccall unsafe "memfd_create"
  c_memfd_create :: CString -> CUInt -> IO CInt

foreign import ccall unsafe "fcntl"
  c_fcntl :: CInt -> CInt -> CInt -> IO CInt

-- Constants for sealing
mfd_allow_sealing :: CUInt
mfd_allow_sealing = 0x0002
f_add_seals, f_seal_shrink, f_seal_grow, f_seal_write, f_seal_seal :: CInt
f_add_seals = 1033
f_seal_shrink = 0x0002
f_seal_grow = 0x0004
f_seal_write = 0x0008
f_seal_seal = 0x0010

createKeymapFd :: String -> IO CInt
createKeymapFd content = do
  -- 1. Create anonymous file in RAM
  withCString "river-keymap" $ \name -> do
    fd <- c_memfd_create name mfd_allow_sealing
    let fd_ = Fd fd

    -- 2. Write the content
    let bytes = castCharToCChar <$> content
    withArrayLen bytes $ \len ptr -> do
      _ <- fdWriteBuf fd_ (castPtr ptr) (fromIntegral len)
      _ <- fdSeek fd_ AbsoluteSeek 0
      -- 3. Seal the file so it's read-only for the compositor
      -- This is required by the river_xkb_config_v1 protocol
      _ <- c_fcntl fd f_add_seals (f_seal_shrink + f_seal_grow + f_seal_write + f_seal_seal)

      return fd
