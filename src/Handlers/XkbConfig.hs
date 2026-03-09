module Handlers.XkbConfig where

import Foreign
import Foreign.C
import GHC.IO.Device (SeekMode (AbsoluteSeek))
import System.Posix.Files (setFdSize)
import System.Posix.IO
import System.Posix.Types (Fd (..))
import Types
import Wayland.Client
import Wayland.ImportedFunctions

foreign export ccall "hs_xkb_config_finished"
  hsXkbConfigFinished :: Ptr () -> Ptr RiverXkbConfig -> IO ()
foreign export ccall "hs_xkb_config_xkb_keyboard"
  hsXkbConfigXkbKeyboard :: Ptr () -> Ptr RiverXkbConfig -> Ptr RiverXkbKeyboard -> IO ()

hsXkbConfigFinished :: Ptr () -> Ptr RiverXkbConfig -> IO ()
hsXkbConfigFinished _ config = riverXkbConfigDestroy config

hsXkbConfigXkbKeyboard :: Ptr () -> Ptr RiverXkbConfig -> Ptr RiverXkbKeyboard -> IO ()
hsXkbConfigXkbKeyboard dataPtr config keyboard = do
  _ <- wlProxyAddListener (castPtr keyboard) getRiverXkbKeyboardListener dataPtr
  riverXkbKeyboardNumlockEnable keyboard
  fd <- createKeymapFd composeKeyMap
  keymap <- riverXkbConfigCreateKeymap config fd 1
  -- _ <- wlProxyAddListener (castPtr keyboard) getRiverXkbKeymapListener dataPtr
  pure ()

-- You'll need to import these from a library like 'unix' or bind them via FFI
foreign import ccall unsafe "memfd_create"
  c_memfd_create :: CString -> CUInt -> IO CInt

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

    -- 2. Write the content
    let bytes = castCharToCChar <$> content
    withArrayLen bytes $ \len ptr -> do
      _ <- fdWriteBuf (Fd fd) (castPtr ptr) (fromIntegral len)

      -- 3. Seal the file so it's read-only for the compositor
      -- This is required by the river_xkb_config_v1 protocol
      _ <- c_fcntl fd f_add_seals (f_seal_shrink + f_seal_grow + f_seal_write + f_seal_seal)

      return fd

foreign import ccall unsafe "fcntl"
  c_fcntl :: CInt -> CInt -> CInt -> IO CInt

composeKeyMap :: String
composeKeyMap =
  "xkb_keymap {\n"
    ++ "    xkb_keycodes  { include \"evdev+aliases(qwerty)\" };\n"
    ++ "    xkb_types     { include \"complete\" };\n"
    ++ "    xkb_compat    { include \"complete\" };\n"
    ++ "    xkb_symbols   { include \"pc+us+inet(evdev)+compose(rctrl)\" };\n"
    ++ "    xkb_geometry  { include \"pc(pc105)\" };\n"
    ++ "};"
