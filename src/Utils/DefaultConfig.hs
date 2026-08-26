module Utils.DefaultConfig where

import Data.Bits ((.|.))
import Data.Map.Strict qualified as M
import Layouts.Basic
import Types
import Utils.KeyDispatches
import Utils.Keysyms

defaultConfig :: RivermonadConfig
defaultConfig =
  RivermonadConfig
    { allPointerBindings =
        M.fromList
          [ ((BtnLeft, ModSuper), (dragWindow, stopDragging))
          , ((BtnRight, ModSuper), (resizeWindow, stopResizing))
          , ((BtnRight, ModSuper .|. ModAlt), (exec "hyprpicker", doNothing))
          ]
    , defaultLayouts =
        M.fromList
          [ (1, monocle)
          , (2, monocle)
          , (3, monocle)
          , (4, monocle)
          , (5, monocle)
          , (6, monocle)
          , (7, monocle)
          , (8, monocle)
          , (9, monocle)
          ]
    , statePath = "/tmp/rivermonad-state.json"
    , floatingRules = []
    , workspaceRules = []
    , windowSizeRules = []
    , execOnStart = []
    , borderPx = 2
    , gapPx = 0
    , borderColor = 0x444444ff
    , focusedBorderColor = 0x7fc8ffff
    , pinnedBorderColor = 0x341539ff
    , xCursorTheme = ("", 24)
    , allKeyBindings =
        M.fromList
          [ ((KeyQ, ModSuper), (closeCurrentWindow))
          , ((KeyF, ModSuper), (toggleFullscreenCurrentWindow))
          , ((KeySpace, ModSuper), (toggleFloatingCurrentWindow))
          , ((KeySpace, modSuperShift), (toggleFocusFloating))
          , ((KeyP, ModSuper), (togglePinWindow))
          , ((Key1, ModSuper), (switchWorkspace 1))
          , ((Key2, ModSuper), (switchWorkspace 2))
          , ((Key3, ModSuper), (switchWorkspace 3))
          , ((Key4, ModSuper), (switchWorkspace 4))
          , ((Key5, ModSuper), (switchWorkspace 5))
          , ((Key6, ModSuper), (switchWorkspace 6))
          , ((Key7, ModSuper), (switchWorkspace 7))
          , ((Key8, ModSuper), (switchWorkspace 8))
          , ((Key9, ModSuper), (switchWorkspace 9))
          , ((KeyKP1, ModSuper), (switchWorkspace 1))
          , ((KeyKP2, ModSuper), (switchWorkspace 2))
          , ((KeyKP3, ModSuper), (switchWorkspace 3))
          , ((KeyKP4, ModSuper), (switchWorkspace 4))
          , ((KeyKP5, ModSuper), (switchWorkspace 5))
          , ((KeyKP6, ModSuper), (switchWorkspace 6))
          , ((KeyKP7, ModSuper), (switchWorkspace 7))
          , ((KeyKP8, ModSuper), (switchWorkspace 8))
          , ((KeyKP9, ModSuper), (switchWorkspace 9))
          , ((Key1, modSuperShift), (moveWindowToWorkspace 1))
          , ((Key2, modSuperShift), (moveWindowToWorkspace 2))
          , ((Key3, modSuperShift), (moveWindowToWorkspace 3))
          , ((Key4, modSuperShift), (moveWindowToWorkspace 4))
          , ((Key5, modSuperShift), (moveWindowToWorkspace 5))
          , ((Key6, modSuperShift), (moveWindowToWorkspace 6))
          , ((Key7, modSuperShift), (moveWindowToWorkspace 7))
          , ((Key8, modSuperShift), (moveWindowToWorkspace 8))
          , ((Key9, modSuperShift), (moveWindowToWorkspace 9))
          , ((KeyKPEnd, modSuperShift), (moveWindowToWorkspace 1))
          , ((KeyKPDown, modSuperShift), (moveWindowToWorkspace 2))
          , ((KeyKPPageDown, modSuperShift), (moveWindowToWorkspace 3))
          , ((KeyKPLeft, modSuperShift), (moveWindowToWorkspace 4))
          , ((KeyKPBegin, modSuperShift), (moveWindowToWorkspace 5))
          , ((KeyKPRight, modSuperShift), (moveWindowToWorkspace 6))
          , ((KeyKPHome, modSuperShift), (moveWindowToWorkspace 7))
          , ((KeyKPUp, modSuperShift), (moveWindowToWorkspace 8))
          , ((KeyKPPageUp, modSuperShift), (moveWindowToWorkspace 9))
          , ((KeyKPEnd, ModSuper), (switchWorkspace 1))
          , ((KeyKPDown, ModSuper), (switchWorkspace 2))
          , ((KeyKPPageDown, ModSuper), (switchWorkspace 3))
          , ((KeyKPLeft, ModSuper), (switchWorkspace 4))
          , ((KeyKPBegin, ModSuper), (switchWorkspace 5))
          , ((KeyKPRight, ModSuper), (switchWorkspace 6))
          , ((KeyKPHome, ModSuper), (switchWorkspace 7))
          , ((KeyKPUp, ModSuper), (switchWorkspace 8))
          , ((KeyKPPageUp, ModSuper), (switchWorkspace 9))
          , ((KeyEqual, ModSuper), (sendMessage (IncMasterFrac 0.04)))
          , ((KeyMinus, ModSuper), (sendMessage (IncMasterFrac (-0.04))))
          , ((KeyLeft, ModSuper), (focusWindow WindowLeft))
          , ((KeyRight, ModSuper), (focusWindow WindowRight))
          , ((KeyUp, ModSuper), (focusWindow WindowUp))
          , ((KeyDown, ModSuper), (focusWindow WindowDown))
          , ((KeyLeft, modSuperShift), (swapWindow WindowLeft))
          , ((KeyRight, modSuperShift), (swapWindow WindowRight))
          , ((KeyUp, modSuperShift), (swapWindow WindowUp))
          , ((KeyDown, modSuperShift), (swapWindow WindowDown))
          , ((KeyDelete, ModControl .|. ModAlt), (exitSession))
          , ((KeyXF86AudioRaiseVolume, ModNone), (exec "wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%+"))
          , ((KeyXF86AudioLowerVolume, ModNone), (exec "wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%-"))
          , ((KeyXF86AudioMute, ModNone), (exec "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))
          , ((KeyXF86AudioMicMute, ModNone), (exec "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"))
          , ((KeyXF86MonBrightnessDown, ModNone), (exec "ddcutil setvcp 10 - 10"))
          , ((KeyXF86MonBrightnessUp, ModNone), (exec "ddcutil setvcp 10 + 10"))
          ]
    , keyboardOptions = HsXkbRuleNames Nothing Nothing Nothing Nothing Nothing
    , keyboardRepeatInfo = Nothing
    }
