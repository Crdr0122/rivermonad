module Config (
  allKeyBindings,
  defaultLayouts,
  defaultRatios,
  borderColor,
  borderPx,
  focusedBorderColor,
  execOnStart,
  allPointerBindings,
  xCursorTheme,
) where

import Data.Bits ((.|.))
import Data.IORef
import Data.Map qualified as M
import Data.Word (Word32)
import Foreign.C (CInt, CUInt)
import Types
import Utils.KeyDispatches
import Utils.Keysyms
import Utils.Layouts

allPointerBindings :: [(CUInt, CUInt, IORef WMState -> IO (), IORef WMState -> IO ())]
allPointerBindings =
  [ (btnLeft, modSuper, dragWindow, stopDragging)
  , (btnRight, modSuper, resizeWindow, stopResizing)
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

defaultRatios :: M.Map WorkspaceID Double
defaultRatios =
  M.fromList $
    zip [1 .. 9] (repeat 0.6)

execOnStart :: [String]
execOnStart =
  [ "foot"
  , "thunar"
  , "foot -e yazi"
  , "waybar -c /home/yu/.config/waybar/mango/config.jsonc -s /home/yu/.config/waybar/mango/style.css"
  , "swaybg -i ~/nixconf/assets/Wallpaper.jpg"
  ]

borderPx :: CInt
borderPx = 2
borderColor, focusedBorderColor :: Word32
borderColor = 0x444444ff
focusedBorderColor = 0x7fc8ffff

cycleWindowsOrSlaves :: Bool -> IORef WMState -> IO ()
cycleWindowsOrSlaves forward stateIORef = do
  state <- readIORef stateIORef
  case layoutName (workspaceLayouts state M.! (focusedWorkspace state)) of
    "TwoPane" -> cycleWindowSlaves forward stateIORef
    _ -> cycleWindows forward stateIORef

xCursorTheme :: (String, CUInt)
xCursorTheme = ("Himehina", 24)

allKeyBindings :: [(CUInt, CUInt, IORef WMState -> IO ())]
allKeyBindings =
  [ (keyQ, modSuper, closeCurrentWindow)
  , (keyTab, modSuper, cycleWindowsOrSlaves True)
  , (keyTab, modSuperShift, cycleWindowsOrSlaves False)
  , (keyW, modSuper, cycleLayout [monocleLayout, twoPaneLayout, stackLayout])
  , (keyF, modSuper, toggleFullscreenCurrentWindow)
  , (keyS, modSuper, zoomWindow)
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
  , (keyKP1, modSuper, switchWorkspace 1)
  , (keyKP2, modSuper, switchWorkspace 2)
  , (keyKP3, modSuper, switchWorkspace 3)
  , (keyKP4, modSuper, switchWorkspace 4)
  , (keyKP5, modSuper, switchWorkspace 5)
  , (keyKP6, modSuper, switchWorkspace 6)
  , (keyKP7, modSuper, switchWorkspace 7)
  , (keyKP8, modSuper, switchWorkspace 8)
  , (keyKP9, modSuper, switchWorkspace 9)
  , (key1, modSuperShift, moveWindowToWorkspace 1 True)
  , (key2, modSuperShift, moveWindowToWorkspace 2 True)
  , (key3, modSuperShift, moveWindowToWorkspace 3 True)
  , (key4, modSuperShift, moveWindowToWorkspace 4 True)
  , (key5, modSuperShift, moveWindowToWorkspace 5 True)
  , (key6, modSuperShift, moveWindowToWorkspace 6 True)
  , (key7, modSuperShift, moveWindowToWorkspace 7 True)
  , (key8, modSuperShift, moveWindowToWorkspace 8 True)
  , (key9, modSuperShift, moveWindowToWorkspace 9 True)
  , (keyKP1, modSuperShift, moveWindowToWorkspace 1 True)
  , (keyKP2, modSuperShift, moveWindowToWorkspace 2 True)
  , (keyKP3, modSuperShift, moveWindowToWorkspace 3 True)
  , (keyKP4, modSuperShift, moveWindowToWorkspace 4 True)
  , (keyKP5, modSuperShift, moveWindowToWorkspace 5 True)
  , (keyKP6, modSuperShift, moveWindowToWorkspace 6 True)
  , (keyKP7, modSuperShift, moveWindowToWorkspace 7 True)
  , (keyKP8, modSuperShift, moveWindowToWorkspace 8 True)
  , (keyKP9, modSuperShift, moveWindowToWorkspace 9 True)
  , (keyEqual, modSuper, modifyLayoutRatio 0.04)
  , (keyMinus, modSuper, modifyLayoutRatio (-0.04))
  ]

modSuperShift :: CUInt
modSuperShift = modSuper .|. modShift
