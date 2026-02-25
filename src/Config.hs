module Config where

import Data.Bits ((.|.))
import Data.IORef
import Data.Map qualified as M
import Data.Word (Word32)
import Foreign.C (CInt, CUInt)
import Types
import Utils.KeyDispatches
import Utils.Keysyms
import Utils.Layouts

allKeyBindings :: [(CUInt, CUInt, IORef WMState -> IO ())]
allKeyBindings =
  [ (keyQ, modSuper, closeCurrentWindow)
  , (keyTab, modSuper, cycleWindows)
  , (keyTab, modSuper .|. modShift, cycleWindowsBack)
  , (keyW, modSuper, cycleLayout [monocleLayout, twoPaneLayout, stackLayout])
  , (keyF, modSuper, toggleFullscreenCurrentWindow)
  , (keySpace, modSuper, toggleFloatingCurrentWindow)
  , (keyEnter, modSuper, exec "foot")
  , (keyZ, modSuper, exec "foot -e yazi")
  , (keyX, modSuper, exec "foot -e nvim")
  , (keyV, modSuper, exec "foot -e calpersonal")
  , (keyB, modSuper, exec "foot -e btop")
  , (keyN, modSuper, exec "foot -e ncmpcpp")
  , (keyM, modSuper, exec "foot -e neomutt")
  , (keyD, modSuper, exec "~/.config/rofi/launcher/launcher.sh")
  , (keyE, modSuper, exec "~/.config/rofi/notification/notification.sh")
  , (keyO, modSuper, exec "~/.config/rofi/password/password.sh")
  , (key1, modSuper, switchWorkspace 1)
  , (key2, modSuper, switchWorkspace 2)
  , (key3, modSuper, switchWorkspace 3)
  , (key4, modSuper, switchWorkspace 4)
  , (key5, modSuper, switchWorkspace 5)
  , (key6, modSuper, switchWorkspace 6)
  , (key7, modSuper, switchWorkspace 7)
  , (key8, modSuper, switchWorkspace 8)
  , (key9, modSuper, switchWorkspace 9)
  , (keyEqual, modSuper, modifyLayoutRatio 4)
  , (keyMinus, modSuper, modifyLayoutRatio (-4))
  ]

defaultLayouts :: M.Map WorkspaceID LayoutType
defaultLayouts =
  M.fromList
    [ (1, twoPaneLayout)
    , (2, stackLayout)
    , (3, monocleLayout)
    , (4, monocleLayout)
    , (5, monocleLayout)
    , (6, monocleLayout)
    , (7, monocleLayout)
    , (8, monocleLayout)
    , (9, monocleLayout)
    ]

defaultRatios :: M.Map WorkspaceID Int
defaultRatios =
  M.fromList $
    zip [1 .. 9] (repeat 60)

execOnStart :: [String]
execOnStart =
  [ "foot"
  ]

borderPx :: CInt
borderPx = 2
borderColor, focusedBorderColor :: Word32
borderColor = 0x444444ff
focusedBorderColor = 0x7fc8ffff
