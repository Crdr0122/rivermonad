module Utils.DefaultConfig (defaultConfig) where

import Data.Map.Strict qualified as M
import Layouts.Basic
import Types
import Utils.Helpers
import Utils.KeyDispatches
import Utils.Keysyms

defaultConfig :: RivermonadConfig
defaultConfig =
  RivermonadConfig
    { borderPx = 2
    , gapPx = 0
    , xCursorTheme = ("", 24)
    , allPointerBindings =
        M.fromList
          [ ((BtnLeft, modSuper), (dragWindow, stopDragging))
          , ((BtnRight, modSuper), (resizeWindow, stopResizing))
          , ((BtnRight, modSuperAlt), (exec "hyprpicker", doNothing))
          ]
    , allKeyBindings =
        M.fromList
          [ ((KeyQ, modSuper), (closeCurrentWindow))
          , ((KeyF, modSuper), (toggleFullscreenCurrentWindow))
          , ((KeySpace, modSuper), (toggleFloatingCurrentWindow))
          , ((KeySpace, modSuperShift), (toggleFocusFloating))
          , ((KeyP, modSuper), (togglePinWindow))
          , ((Key1, modSuper), (switchWorkspace 1))
          , ((Key2, modSuper), (switchWorkspace 2))
          , ((Key3, modSuper), (switchWorkspace 3))
          , ((Key4, modSuper), (switchWorkspace 4))
          , ((Key5, modSuper), (switchWorkspace 5))
          , ((Key6, modSuper), (switchWorkspace 6))
          , ((Key7, modSuper), (switchWorkspace 7))
          , ((Key8, modSuper), (switchWorkspace 8))
          , ((Key9, modSuper), (switchWorkspace 9))
          , ((KeyKP1, modSuper), (switchWorkspace 1))
          , ((KeyKP2, modSuper), (switchWorkspace 2))
          , ((KeyKP3, modSuper), (switchWorkspace 3))
          , ((KeyKP4, modSuper), (switchWorkspace 4))
          , ((KeyKP5, modSuper), (switchWorkspace 5))
          , ((KeyKP6, modSuper), (switchWorkspace 6))
          , ((KeyKP7, modSuper), (switchWorkspace 7))
          , ((KeyKP8, modSuper), (switchWorkspace 8))
          , ((KeyKP9, modSuper), (switchWorkspace 9))
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
          , ((KeyKPEnd, modSuper), (switchWorkspace 1))
          , ((KeyKPDown, modSuper), (switchWorkspace 2))
          , ((KeyKPPageDown, modSuper), (switchWorkspace 3))
          , ((KeyKPLeft, modSuper), (switchWorkspace 4))
          , ((KeyKPBegin, modSuper), (switchWorkspace 5))
          , ((KeyKPRight, modSuper), (switchWorkspace 6))
          , ((KeyKPHome, modSuper), (switchWorkspace 7))
          , ((KeyKPUp, modSuper), (switchWorkspace 8))
          , ((KeyKPPageUp, modSuper), (switchWorkspace 9))
          , ((KeyEqual, modSuper), (sendMessage (IncMasterFrac 0.04)))
          , ((KeyMinus, modSuper), (sendMessage (IncMasterFrac (-0.04))))
          , ((KeyLeft, modSuper), (focusWindow WindowLeft))
          , ((KeyRight, modSuper), (focusWindow WindowRight))
          , ((KeyUp, modSuper), (focusWindow WindowUp))
          , ((KeyDown, modSuper), (focusWindow WindowDown))
          , ((KeyLeft, modSuperShift), (swapWindow WindowLeft))
          , ((KeyRight, modSuperShift), (swapWindow WindowRight))
          , ((KeyUp, modSuperShift), (swapWindow WindowUp))
          , ((KeyDown, modSuperShift), (swapWindow WindowDown))
          , ((KeyDelete, modCtrlAlt), (exitSession))
          , ((KeyXF86AudioRaiseVolume, modNone), (exec "wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%+"))
          , ((KeyXF86AudioLowerVolume, modNone), (exec "wpctl set-volume -l 1 @DEFAULT_AUDIO_SINK@ 2%-"))
          , ((KeyXF86AudioMute, modNone), (exec "wpctl set-mute @DEFAULT_AUDIO_SINK@ toggle"))
          , ((KeyXF86AudioMicMute, modNone), (exec "wpctl set-mute @DEFAULT_AUDIO_SOURCE@ toggle"))
          , ((KeyXF86MonBrightnessDown, modNone), (exec "ddcutil setvcp 10 - 10"))
          , ((KeyXF86MonBrightnessUp, modNone), (exec "ddcutil setvcp 10 + 10"))
          ]
    , workspaceRules = []
    , floatingRules = []
    , windowSizeRules = []
    , borderColor = 0x444444ff
    , focusedBorderColor = 0x7fc8ffff
    , pinnedBorderColor = 0x341539ff
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
    , execOnStart = []
    , keyboardOptions = HsXkbRuleNames Nothing Nothing Nothing Nothing Nothing
    , keyboardRepeatInfo = Nothing
    }
