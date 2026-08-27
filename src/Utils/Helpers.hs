{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RecordWildCards #-}

module Utils.Helpers (
  calculateFloatingPosition,
  calculateFloatingPositions,
  workspaceWindows,
  focusedWorkspace,
  setFocusedWindowAndHistory,
  focusedOutputGeom,
  pairOfGetter,
  pairOf,
  deleteWinPtrs,
  rmlvoToKeymapFd,
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
  Window{floatingGeometry, nodePtr, dimensionsHint, ruleSize}
  Rect{rh = outHeight, rw = outWidth, rx = outX, ry = outY} =
    let (resX, resY, resW, resH) = case floatingGeometry of
          Just Rect{rx, ry, rw, rh} -> (rx, ry, rw, rh)
          Nothing -> case ruleSize of
            Just (rw, rh) -> ((outWidth - rw) `div` 2 + dx, (outHeight - rh) `div` 2 + dy, rw, rh)
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

-- XkbKeymap Creation Stuff
foreign import ccall unsafe "memfd_create"
  c_memfd_create :: CString -> CUInt -> IO CInt

foreign import ccall unsafe "fcntl"
  c_fcntl :: CInt -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "strlen"
  c_strlen :: CString -> IO CSize

foreign import capi "xkbcommon/xkbcommon.h xkb_context_new"
  xkb_context_new :: CUInt -> IO (Ptr XkbContext)

foreign import capi "xkbcommon/xkbcommon.h xkb_keymap_new_from_names"
  xkb_keymap_new_from_names :: Ptr XkbContext -> Ptr XkbRuleNames -> CUInt -> IO (Ptr XkbKeymap)

foreign import capi "xkbcommon/xkbcommon.h xkb_keymap_get_as_string"
  xkb_keymap_get_as_string :: Ptr XkbKeymap -> CUInt -> IO CString

foreign import capi "xkbcommon/xkbcommon.h xkb_keymap_unref"
  xkb_keymap_unref :: Ptr XkbKeymap -> IO ()

foreign import capi "xkbcommon/xkbcommon.h xkb_context_unref"
  xkb_context_unref :: Ptr XkbContext -> IO ()

data XkbRuleNames = XkbRuleNames
  { _xkbRules :: CString
  , _xkbModel :: CString
  , _xkbLayout :: CString
  , _xkbVariant :: CString
  , _xkbOptions :: CString
  }

instance Storable XkbRuleNames where
  sizeOf _ = sizeOf (nullPtr :: CString) * 5
  alignment _ = alignment (nullPtr :: CString)
  peek ptr = do
    r <- peekByteOff ptr (0 * sz)
    m <- peekByteOff ptr (1 * sz)
    l <- peekByteOff ptr (2 * sz)
    v <- peekByteOff ptr (3 * sz)
    o <- peekByteOff ptr (4 * sz)
    pure $ XkbRuleNames r m l v o
   where
    sz = sizeOf (nullPtr :: CString)
  poke ptr (XkbRuleNames r m l v o) = do
    pokeByteOff ptr (0 * sz) r
    pokeByteOff ptr (1 * sz) m
    pokeByteOff ptr (2 * sz) l
    pokeByteOff ptr (3 * sz) v
    pokeByteOff ptr (4 * sz) o
   where
    sz = sizeOf (nullPtr :: CString)

rmlvoToKeymapFd :: HsXkbRuleNames -> IO (Maybe CInt)
rmlvoToKeymapFd HsXkbRuleNames{..} = do
  ctx <- xkb_context_new 0
  if ctx == nullPtr
    then pure Nothing
    else do
      let withNullableStr mStr act = case mStr of
            Nothing -> act nullPtr
            Just s -> withCString s act
      withNullableStr hsXkbRules $ \cRules ->
        withNullableStr hsXkbModel $ \cModel ->
          withNullableStr hsXkbLayout $ \cLayout ->
            withNullableStr hsXkbVariant $ \cVariant ->
              withNullableStr hsXkbOptions $ \cOptions -> do
                let names = XkbRuleNames cRules cModel cLayout cVariant cOptions
                with names $ \namesPtr -> do
                  keymap <- xkb_keymap_new_from_names ctx namesPtr 0
                  if keymap == nullPtr
                    then do
                      xkb_context_unref ctx
                      pure Nothing
                    else do
                      cKeymapStr <- xkb_keymap_get_as_string keymap 1 -- XKB_KEYMAP_FORMAT_TEXT_V1 = 1
                      fd <- createKeymapFd cKeymapStr
                      -- Clean up C allocations
                      free cKeymapStr
                      xkb_keymap_unref keymap
                      xkb_context_unref ctx

                      pure (Just fd)

-- Constants for sealing
mfd_allow_sealing :: CUInt
mfd_allow_sealing = 0x0002
f_add_seals, f_seal_shrink, f_seal_grow, f_seal_write, f_seal_seal :: CInt
f_add_seals = 1033
f_seal_shrink = 0x0002
f_seal_grow = 0x0004
f_seal_write = 0x0008
f_seal_seal = 0x0010

-- createKeymapFd :: String -> IO CInt
-- createKeymapFd content = do
--   -- 1. Create anonymous file in RAM
--   withCString "river-keymap" $ \name -> do
--     fd <- c_memfd_create name mfd_allow_sealing
--     let fd_ = Fd fd
--
--     -- 2. Write the content
--     let bytes = castCharToCChar <$> content
--     withArrayLen bytes $ \len ptr -> do
--       _ <- fdWriteBuf fd_ (castPtr ptr) (fromIntegral len)
--       _ <- fdSeek fd_ AbsoluteSeek 0
--       -- 3. Seal the file so it's read-only for the compositor
--       -- This is required by the river_xkb_config_v1 protocol
--       _ <- c_fcntl fd f_add_seals (f_seal_shrink + f_seal_grow + f_seal_write + f_seal_seal)
--
--       return fd

createKeymapFd :: CString -> IO CInt
createKeymapFd cStr = do
  -- 1. Create anonymous file in RAM
  len <- c_strlen cStr
  fd <- withCString "river-keymap" $ \name -> c_memfd_create name mfd_allow_sealing
  let fd_ = Fd fd
  _ <- fdWriteBuf fd_ (castPtr cStr) (fromIntegral len)
  _ <- fdSeek fd_ AbsoluteSeek 0
  _ <- c_fcntl fd f_add_seals (f_seal_shrink + f_seal_grow + f_seal_write + f_seal_seal)
  -- 2. Write the content

  return fd
