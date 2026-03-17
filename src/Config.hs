module Config (myConfig) where

import Control.Concurrent.MVar
import Data.Bimap qualified as B
import Data.Map.Strict qualified as M
import Foreign
import Types
import Utils.DefaultConfig
import Utils.KeyDispatches
import Utils.Keysyms
import Utils.Layouts

myLayout :: Int -> SomeLayout
myLayout i = choose i [monocle, tall 0.6 1, twoPane 0.6 1]

myConfig :: RivermonadConfig
myConfig =
  defaultConfig
    { allPointerBindings =
        M.union
          (M.fromList [((btnRight, modSuper .|. modAlt), (exec "hyprpicker", doNothing))])
          (allPointerBindings defaultConfig)
    , defaultLayouts =
        M.fromList
          [ (1, myLayout 0)
          , (2, myLayout 1)
          , (3, myLayout 2)
          , (4, myLayout 0)
          , (5, myLayout 0)
          , (6, myLayout 0)
          , (7, myLayout 0)
          , (8, myLayout 0)
          , (9, myLayout 0)
          ]
    , xCursorTheme = ("Himehina", 24)
    , workspaceRules =
        [ ("", "Slack", 2)
        , ("QQ", "QQ", 2)
        , ("Weixin", "wechat", 2)
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
        , ("Photos and Videos", "wechat", Floating)
        , ("QQ", "QQ", Tiled)
        , ("", "QQ", Floating)
        ]
    , allKeyBindings =
        M.union
          ( M.fromList
              [ ((keyTab, modSuper), (cycleWindowsOrSlavesOrFocus True))
              , ((keyTab, modSuperShift), (cycleWindowsOrSlavesOrFocus False))
              , ((keyGrave, modSuper), (startRepeating $ cycleWindowFocus True))
              , ((keyGrave, modSuperShift), (cycleWindowFocus False))
              , ((keyW, modSuper), (sendMessage Next))
              , ((keyS, modSuper), (zoomWindow))
              , ((keyR, modSuperShift), (reloadWindowManager (statePath defaultConfig)))
              , ((keyP, modSuper), (togglePinWindow))
              , ((keyF, modSuperShift), (toggleMaximizeWindow))
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
              , ((keyU, modSuper), (exec "screenrecorder toggle fullscreen"))
              , ((keyU, modSuperShift), (exec "screenrecorder toggle region"))
              , ((keyXF86Calculator, modSuper), (exec "~/.config/river/screenshot fullscreen"))
              , ((keyXF86Calculator, modSuperShift), (exec "~/.config/river/screenshot region"))
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

cycleWindowsOrSlaves :: Bool -> Ptr RiverSeat -> MVar WMState -> IO ()
cycleWindowsOrSlaves forward seat stateMVar = do
  state <- readMVar stateMVar
  case layoutName' (workspaceLayouts state M.! (allOutputWorkspaces state B.! focusedOutput state)) of
    "TwoPane" -> cycleWindowSlaves forward seat stateMVar
    _ -> cycleWindows forward seat stateMVar

cycleWindowsOrSlavesOrFocus :: Bool -> Ptr RiverSeat -> MVar WMState -> IO ()
cycleWindowsOrSlavesOrFocus forward seat stateMVar = do
  state <- readMVar stateMVar
  case focusedWindow state of
    Nothing -> pure ()
    Just w -> do
      let Window{isFloating, isFullscreen} = (allWindows state M.! w)
      if isFloating || isFullscreen
        then cycleWindowFocus forward seat stateMVar
        else cycleWindowsOrSlaves forward seat stateMVar
