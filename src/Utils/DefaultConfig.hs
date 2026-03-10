module Utils.DefaultConfig where

import Data.Bits ((.|.))
import Data.Map.Strict qualified as M
import Types
import Utils.KeyDispatches
import Utils.Keysyms
import Utils.Layouts

defaultConfig :: RivermonadConfig
defaultConfig =
  RivermonadConfig
    { allPointerBindings =
        M.fromList
          [ ((btnLeft, modSuper), (dragWindow, stopDragging))
          , ((btnRight, modSuper), (resizeWindow, stopResizing))
          , ((btnRight, modSuper .|. modAlt), (exec "hyprpicker", doNothing))
          ]
    , defaultLayouts =
        M.fromList
          [ (1, monocleLayout)
          , (2, monocleLayout)
          , (3, monocleLayout)
          , (4, monocleLayout)
          , (5, monocleLayout)
          , (6, monocleLayout)
          , (7, monocleLayout)
          , (8, monocleLayout)
          , (9, monocleLayout)
          ]
    , defaultRatios = M.fromList $ zip [1 .. 9] (repeat 0.6)
    , statePath = "/tmp/rivermonad-state.json"
    , floatingRules = []
    , workspaceRules = []
    , execOnStart = []
    , borderPx = 2
    , gapPx = 0
    , borderColor = 0x444444ff
    , focusedBorderColor = 0x7fc8ffff
    , pinnedBorderColor = 0x341539ff
    , xCursorTheme = ("", 24)
    , allKeyBindings =
        M.fromList
          [ ((keyQ, modSuper), (closeCurrentWindow))
          , ((keyF, modSuper), (toggleFullscreenCurrentWindow))
          , ((keySpace, modSuper), (toggleFloatingCurrentWindow))
          , ((keySpace, modSuperShift), (toggleFocusFloating))
          , ((keyP, modSuper), (togglePinWindow))
          , ((key1, modSuper), (switchWorkspace 1))
          , ((key2, modSuper), (switchWorkspace 2))
          , ((key3, modSuper), (switchWorkspace 3))
          , ((key4, modSuper), (switchWorkspace 4))
          , ((key5, modSuper), (switchWorkspace 5))
          , ((key6, modSuper), (switchWorkspace 6))
          , ((key7, modSuper), (switchWorkspace 7))
          , ((key8, modSuper), (switchWorkspace 8))
          , ((key9, modSuper), (switchWorkspace 9))
          , ((keyKP1, modSuper), (switchWorkspace 1))
          , ((keyKP2, modSuper), (switchWorkspace 2))
          , ((keyKP3, modSuper), (switchWorkspace 3))
          , ((keyKP4, modSuper), (switchWorkspace 4))
          , ((keyKP5, modSuper), (switchWorkspace 5))
          , ((keyKP6, modSuper), (switchWorkspace 6))
          , ((keyKP7, modSuper), (switchWorkspace 7))
          , ((keyKP8, modSuper), (switchWorkspace 8))
          , ((keyKP9, modSuper), (switchWorkspace 9))
          , ((key1, modSuperShift), (moveWindowToWorkspace 1))
          , ((key2, modSuperShift), (moveWindowToWorkspace 2))
          , ((key3, modSuperShift), (moveWindowToWorkspace 3))
          , ((key4, modSuperShift), (moveWindowToWorkspace 4))
          , ((key5, modSuperShift), (moveWindowToWorkspace 5))
          , ((key6, modSuperShift), (moveWindowToWorkspace 6))
          , ((key7, modSuperShift), (moveWindowToWorkspace 7))
          , ((key8, modSuperShift), (moveWindowToWorkspace 8))
          , ((key9, modSuperShift), (moveWindowToWorkspace 9))
          , ((keyKPEnd, modSuperShift), (moveWindowToWorkspace 1))
          , ((keyKPDown, modSuperShift), (moveWindowToWorkspace 2))
          , ((keyKPPageDown, modSuperShift), (moveWindowToWorkspace 3))
          , ((keyKPLeft, modSuperShift), (moveWindowToWorkspace 4))
          , ((keyKPBegin, modSuperShift), (moveWindowToWorkspace 5))
          , ((keyKPRight, modSuperShift), (moveWindowToWorkspace 6))
          , ((keyKPHome, modSuperShift), (moveWindowToWorkspace 7))
          , ((keyKPUp, modSuperShift), (moveWindowToWorkspace 8))
          , ((keyKPPageUp, modSuperShift), (moveWindowToWorkspace 9))
          , ((keyKPEnd, modSuper), (switchWorkspace 1))
          , ((keyKPDown, modSuper), (switchWorkspace 2))
          , ((keyKPPageDown, modSuper), (switchWorkspace 3))
          , ((keyKPLeft, modSuper), (switchWorkspace 4))
          , ((keyKPBegin, modSuper), (switchWorkspace 5))
          , ((keyKPRight, modSuper), (switchWorkspace 6))
          , ((keyKPHome, modSuper), (switchWorkspace 7))
          , ((keyKPUp, modSuper), (switchWorkspace 8))
          , ((keyKPPageUp, modSuper), (switchWorkspace 9))
          , ((keyEqual, modSuper), (modifyLayoutRatio 0.04))
          , ((keyMinus, modSuper), (modifyLayoutRatio (-0.04)))
          ]
    , composeKeyMap =
        "xkb_keymap {\
        \    xkb_keycodes  { include \"evdev+aliases(qwerty)\" };\
        \    xkb_types     { include \"complete\" };\
        \    xkb_compat    { include \"complete\" };\
        \    xkb_symbols   { include \"pc+us+inet(evdev)\" };\
        \    xkb_geometry  { include \"pc(pc105)\" };\
        \};\n"
    }
