{-# LANGUAGE CApiFFI #-}
{-# LANGUAGE RecordWildCards #-}

module Utils.Keymap (rmlvoToKeymapFd) where

import Foreign
import Foreign.C
import System.IO
import System.Posix.IO
import System.Posix.Types (Fd (..))
import Types

data XkbContext
data XkbKeymap

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
  xkb_context_new 0 >>= \case
    ctx | ctx == nullPtr -> pure Nothing
    ctx -> do
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

createKeymapFd :: CString -> IO CInt
createKeymapFd cStr = do
  -- 1. Create anonymous file in RAM
  len <- c_strlen cStr
  fd <- withCString "river-keymap" $ \name -> c_memfd_create name mfd_allow_sealing
  let fd_ = Fd fd
  _ <- fdWriteBuf fd_ (castPtr cStr) (fromIntegral len)
  _ <- fdSeek fd_ AbsoluteSeek 0
  _ <- c_fcntl fd f_add_seals (f_seal_shrink + f_seal_grow + f_seal_write + f_seal_seal)

  return fd
