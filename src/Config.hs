module Config where

import Data.Map qualified as M
import Foreign
import Types
import Wayland.Protocol.ImportedFunctions

allKeyBindings :: M.Map (Word32, Word32) (WMState -> IO (WMState))
allKeyBindings = M.fromList []
