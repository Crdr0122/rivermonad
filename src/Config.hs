module Config (
  allKeyBindings,
  defaultLayouts,
  defaultRatios,
  borderColor,
  borderPx,
  focusedBorderColor,
  pinnedBorderColor,
  execOnStart,
  gapPx,
  allPointerBindings,
  xCursorTheme,
  workspaceRules,
  statePath,
  floatingRules,
) where

import Control.Concurrent.MVar
import Data.Bimap qualified as B
import Data.Bits ((.|.))
import Data.Map.Strict qualified as M
import Data.Word (Word32)
import Foreign.C (CInt, CUInt)
import Types
import Utils.KeyDispatches
import Utils.Keysyms
import Utils.Layouts

allPointerBindings :: [(CUInt, CUInt, MVar WMState -> IO (), MVar WMState -> IO ())]
allPointerBindings =
  [ (btnLeft, modSuper, dragWindow, stopDragging)
  , (btnRight, modSuper, resizeWindow, stopResizing)
  , (btnRight, modSuper .|. modAlt, exec "hyprpicker", doNothing)
  ]

defaultLayouts :: M.Map WorkspaceID LayoutType
defaultLayouts =
  M.fromList
    [ (1, monocleLayout)
    , (2, stackLayout)
    , (3, twoPaneLayout)
    , (4, monocleLayout)
    , (5, monocleLayout)
    , (6, monocleLayout)
    , (7, monocleLayout)
    , (8, circleLayout)
    , (9, roledexLayout)
    ]

defaultRatios :: M.Map WorkspaceID Double
defaultRatios =
  M.fromList $
    zip [1 .. 9] (repeat 0.6)

execOnStart :: [String]
execOnStart = []

borderPx :: CInt
borderPx = 2
borderColor, focusedBorderColor, pinnedBorderColor :: Word32
borderColor = 0x444444ff
focusedBorderColor = 0x7fc8ffff
pinnedBorderColor = 0x341539ff

gapPx :: CInt
gapPx = 0

xCursorTheme :: (String, CUInt)
xCursorTheme = ("Himehina", 24)

modSuperShift :: CUInt
modSuperShift = modSuper .|. modShift

workspaceRules :: [(String, String, WorkspaceID)]
workspaceRules =
  [ ("Thunar", "", 2)
  , ("", "Slack", 2)
  , ("QQ", "QQ", 2)
  , ("", "wechat", 2)
  , ("", "vesktop", 2)
  ]

floatingRules :: [(String, String, Bool)]
floatingRules =
  [ ("Rename ", "thunar", True)
  , ("", "blueman-manager", True)
  , ("", "th123.exe", True)
  , ("Authentication Required", "", True)
  , ("", "sokulauncher.exe", True)
  , ("", "swarm.exe", True)
  , ("", "snapgene.exe", True)
  , ("", "prism.exe", True)
  , ("", "fiji-Main", True)
  , ("SnapGene", "", True)
  , ("", "beatoraja", True)
  , ("QQ", "QQ", False)
  , ("", "QQ", True)
  ]

statePath :: FilePath
statePath = "/tmp/rivermonad-state.json"

allKeyBindings :: [(CUInt, CUInt, MVar WMState -> IO ())]
allKeyBindings =
  [ (keyQ, modSuper, closeCurrentWindow)
  , (keyTab, modSuper, cycleWindowsOrSlavesOrFocus True)
  , (keyTab, modSuperShift, cycleWindowsOrSlavesOrFocus False)
  , (keyGrave, modSuper, cycleWindowFocus True)
  , (keyGrave, modSuperShift, cycleWindowFocus False)
  , (keyW, modSuper, cycleLayout [monocleLayout, twoPaneLayout, stackLayout])
  , (keyF, modSuper, toggleFullscreenCurrentWindow)
  , (keyS, modSuper, zoomWindow)
  , (keyR, modSuperShift, reloadWindowManager statePath)
  , (keySpace, modSuper, toggleFloatingCurrentWindow)
  , (keySpace, modSuperShift, toggleFocusFloating)
  , (keyP, modSuper, togglePinWindow)
  , (keyEnter, modSuper, exec "foot")
  , (keyZ, modSuper, exec "foot -e yazi")
  , (keyX, modSuper, exec "foot -e nvim")
  , (keyV, modSuper, exec "foot -e calpersonal")
  , (keyB, modSuper, exec "foot -e btop")
  , (keyN, modSuper, exec "foot -e ncmpcpp")
  , (keyM, modSuper, exec "foot -e neomutt")
  , (keyA, modSuper, exec "firefox")
  , (keyD, modSuper, exec "~/.config/rofi/launcher/launcher.sh")
  , (keyE, modSuper, exec "~/.config/rofi/notification/notification.sh")
  , (keyO, modSuper, exec "~/.config/rofi/password/password.sh")
  , (keyI, modSuper, exec "~/.config/rofi/mirror/mirror.sh")
  , (keyC, modSuper, exec "~/.config/rofi/powermenu/powermenu.sh")
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
  , (key1, modSuperShift, moveWindowToWorkspace 1)
  , (key2, modSuperShift, moveWindowToWorkspace 2)
  , (key3, modSuperShift, moveWindowToWorkspace 3)
  , (key4, modSuperShift, moveWindowToWorkspace 4)
  , (key5, modSuperShift, moveWindowToWorkspace 5)
  , (key6, modSuperShift, moveWindowToWorkspace 6)
  , (key7, modSuperShift, moveWindowToWorkspace 7)
  , (key8, modSuperShift, moveWindowToWorkspace 8)
  , (key9, modSuperShift, moveWindowToWorkspace 9)
  , (keyKPEnd, modSuperShift, moveWindowToWorkspace 1)
  , (keyKPDown, modSuperShift, moveWindowToWorkspace 2)
  , (keyKPPageDown, modSuperShift, moveWindowToWorkspace 3)
  , (keyKPLeft, modSuperShift, moveWindowToWorkspace 4)
  , (keyKPBegin, modSuperShift, moveWindowToWorkspace 5)
  , (keyKPRight, modSuperShift, moveWindowToWorkspace 6)
  , (keyKPHome, modSuperShift, moveWindowToWorkspace 7)
  , (keyKPUp, modSuperShift, moveWindowToWorkspace 8)
  , (keyKPPageUp, modSuperShift, moveWindowToWorkspace 9)
  , -- , (keyLeft, modSuperShift, moveWindowToWorkspace 9 True)
    -- , (keyRight, modSuperShift, moveWindowToWorkspace 9 True)
    -- , (keyUp, modSuperShift, moveWindowToWorkspace 9 True)
    -- , (keyDown, modSuperShift, moveWindowToWorkspace 9 True)
    (keyEqual, modSuper, modifyLayoutRatio 0.04)
  , (keyMinus, modSuper, modifyLayoutRatio (-0.04))
  ]

cycleWindowsOrSlaves :: Bool -> MVar WMState -> IO ()
cycleWindowsOrSlaves forward stateMVar = do
  state <- readMVar stateMVar
  case layoutName (workspaceLayouts state M.! (allOutputWorkspaces state B.! focusedOutput state)) of
    "TwoPane" -> cycleWindowSlaves forward stateMVar
    _ -> cycleWindows forward stateMVar

cycleWindowsOrSlavesOrFocus :: Bool -> MVar WMState -> IO ()
cycleWindowsOrSlavesOrFocus forward stateMVar = do
  state <- readMVar stateMVar
  case focusedWindow state of
    Nothing -> pure ()
    Just w -> do
      let Window{isFloating, isFullscreen} = (allWindows state M.! w)
      if isFloating || isFullscreen
        then cycleWindowFocus forward stateMVar
        else cycleWindowsOrSlaves forward stateMVar
