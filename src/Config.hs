module Config (
  myConfig,
) where

import Control.Concurrent.MVar
import Data.Bimap qualified as B
import Data.Bits ((.|.))
import Data.Map.Strict qualified as M
import Types
import Utils.DefaultConfig
import Utils.KeyDispatches
import Utils.Keysyms
import Utils.Layouts

myConfig :: RivermonadConfig
myConfig =
  defaultConfig
    { allPointerBindings =
        M.union
          (M.fromList [((btnRight, modSuper .|. modAlt), (exec "hyprpicker", doNothing))])
          (allPointerBindings defaultConfig)
    , defaultLayouts =
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
    , xCursorTheme = ("Himehina", 24)
    , workspaceRules =
        [ ("Thunar", "", 2)
        , ("", "Slack", 2)
        , ("QQ", "QQ", 2)
        , ("", "wechat", 2)
        , ("", "vesktop", 2)
        ]
    , floatingRules =
        [ ("Rename ", "thunar", Floating)
        , ("", "blueman-manager", Floating)
        , ("", "th123.exe", Floating)
        , ("Authentication Required", "", Floating)
        , ("", "sokulauncher.exe", Floating)
        , ("", "swarm.exe", Floating)
        , ("", "snapgene.exe", Floating)
        , ("", "prism.exe", Floating)
        , ("", "fiji-Main", Floating)
        , ("SnapGene", "", Floating)
        , ("", "beatoraja", Floating)
        , ("QQ", "QQ", Tiled)
        , ("", "QQ", Floating)
        ]
    , allKeyBindings =
        M.union
          ( M.fromList
              [ ((keyTab, modSuper), (cycleWindowsOrSlavesOrFocus True))
              , ((keyTab, modSuperShift), (cycleWindowsOrSlavesOrFocus False))
              , ((keyGrave, modSuper), (cycleWindowFocus True))
              , ((keyGrave, modSuperShift), (cycleWindowFocus False))
              , ((keyW, modSuper), (cycleLayout [monocleLayout, twoPaneLayout, stackLayout]))
              , ((keyS, modSuper), (zoomWindow))
              , ((keyR, modSuperShift), (reloadWindowManager (statePath defaultConfig)))
              , ((keyP, modSuper), (togglePinWindow))
              , ((keyEnter, modSuper), (exec "foot"))
              , ((keyZ, modSuper), (exec "foot -e yazi"))
              , ((keyX, modSuper), (exec "foot -e nvim"))
              , ((keyV, modSuper), (exec "foot -e calpersonal"))
              , ((keyB, modSuper), (exec "foot -e btop"))
              , ((keyN, modSuper), (exec "foot -e ncmpcpp"))
              , ((keyM, modSuper), (exec "foot -e neomutt"))
              , ((keyA, modSuper), (exec "firefox"))
              , ((keyD, modSuper), (exec "~/.config/rofi/launcher/launcher.sh"))
              , ((keyE, modSuper), (exec "~/.config/rofi/notification/notification.sh"))
              , ((keyO, modSuper), (exec "~/.config/rofi/password/password.sh"))
              , ((keyI, modSuper), (exec "~/.config/rofi/mirror/mirror.sh"))
              , ((keyC, modSuper), (exec "~/.config/rofi/powermenu/powermenu.sh"))
              -- , (keyLeft, modSuperShift, moveWindowToWorkspace 9 True)
              -- , (keyRight, modSuperShift, moveWindowToWorkspace 9 True)
              -- , (keyUp, modSuperShift, moveWindowToWorkspace 9 True)
              -- , (keyDown, modSuperShift, moveWindowToWorkspace 9 True)
              ]
          )
          (allKeyBindings defaultConfig)
    , composeKeyMap =
        "xkb_keymap {\
        \    xkb_keycodes  { include \"evdev+aliases(qwerty)\" };\
        \    xkb_types     { include \"complete\" };\
        \    xkb_compat    { include \"complete\" };\
        \    xkb_symbols   { include \"pc+us+inet(evdev)+compose(rctrl)\" };\
        \    xkb_geometry  { include \"pc(pc105)\" };\
        \};\n"
    }

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
