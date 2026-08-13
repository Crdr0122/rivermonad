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
  setMinSize,
  deleteWinPtrs,
) where

import Control.Monad.State
import Data.Bimap qualified as B
import Data.List qualified as L
import Data.Map qualified as M
import Data.Sequence qualified as S
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

deleteWinPtrs :: (MonadState WMState m) => Ptr RiverWindow -> m ()
deleteWinPtrs win = do
  #allWorkspacesFloating %= BS.delete win
  #allWorkspacesTiled %= BS.delete win
  #allWorkspacesFullscreen %= BS.delete win
  #newWindowQueue %= L.delete win
  #floatingQueue %= M.map (filter (/= win))
  #fullscreenQueue %= M.map (filter (/= win))
  #workspaceFocusHistory %= M.filter (/= win)

setMinSize :: Lens (a, b, c, d) (a', b', c, d) (a, b) (a', b')
setMinSize =
  lens
    (\(a, b, _, _) -> (a, b)) -- Getter
    (\(_, _, c, d) (a', b') -> (a', b', c, d)) -- Setter

calculateFloatingPositions :: Rect -> [Window] -> Int -> ([Rect], IO (), IO ())
calculateFloatingPositions o windows num = result
 where
  resultList = fmap (\(n, win) -> calculateFloatingPosition (winPtr win) n win o) (zip [num ..] windows)
  result =
    foldl'
      (\(rects, ms, rs) (rect, m, r) -> (rect : rects, ms >> m, rs >> r))
      ([], pure (), pure ())
      resultList

calculateFloatingPosition :: Ptr RiverWindow -> Int -> Window -> Rect -> (Rect, IO (), IO ())
calculateFloatingPosition
  win
  num
  Window{floatingGeometry, nodePtr, dimensionsHint}
  Rect{rh = outHeight, rw = outWidth, rx = outX, ry = outY} =
    let (resX, resY, resW, resH) = case floatingGeometry of
          Just Rect{rx, ry, rw, rh} -> (rx, ry, rw, rh)
          Nothing -> case dimensionsHint of
            (0, 0, _, _) -> (offsetX + dx, offsetY + dy, w, h)
            (minW, minH, 0, 0) ->
              let
                maxW = max minW w
                maxH = max minH h
                minY = (outHeight - maxH) `div` 2
                minX = (outWidth - maxW) `div` 2
               in
                (minX + dx, minY + dy, maxW, maxH)
            (_, _, maxW, maxH) ->
              let
                minW = min maxW w
                minH = min maxH h
                maxY = (outHeight - minH) `div` 2
                maxX = (outWidth - minW) `div` 2
               in
                (maxX + dx, maxY + dy, minW, minH)
     in ( Rect{rx = resX, ry = resY, rw = resW, rh = resH}
        , riverWindowProposeDimensions win resW resH
        , riverNodeSetPosition nodePtr (outX + resX) (outY + resY) >> riverNodePlaceTop nodePtr
        )
   where
    w = outWidth * 6 `div` 10
    h = outHeight * 6 `div` 10
    offsetX = (outWidth - w) `div` 2
    offsetY = (outHeight - h) `div` 2
    -- Bounded scatter offsets based on `num`
    step = num `mod` 8
    scaleX = min 36 (outWidth `div` 30)
    scaleY = min 28 (outHeight `div` 30)

    -- Scatters in all 4 directions around center (X, Y multipliers)
    (multX, multY) = case step of
      0 -> (0, 0) -- Center
      1 -> (1, 1) -- Bottom-Right
      2 -> (-1, 1) -- Bottom-Left
      3 -> (1, -1) -- Top-Right
      4 -> (-1, -1) -- Top-Left
      5 -> (2, 0) -- Far-Right
      6 -> (0, 2) -- Far-Bottom
      7 -> (-2, -2) -- Far-Top-Left
      _ -> (0, 0)

    dx = multX * scaleX
    dy = multY * scaleY

workspaceWindows :: WorkspaceID -> Getter WMState (S.Seq (Ptr RiverWindow))
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
