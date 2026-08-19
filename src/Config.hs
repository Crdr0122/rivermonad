module Config (myConfig) where

import Control.Concurrent.MVar
import Data.Bimap qualified as B
import Data.Map.Strict qualified as M
import Foreign
import Layouts.Basic
import Layouts.Magnifier
import Layouts.Overview
import Types
import Utils.DefaultConfig
import Utils.KeyDispatches
import Utils.Keysyms

myLayout :: Int -> SomeLayout
myLayout i = overview False $ choose i [monocle, magnifier2' 1.5 (tall 0.6 1), twoPane 0.6]

myConfig :: RivermonadConfig
myConfig =
  defaultConfig
    { allPointerBindings =
        M.union
          (M.fromList [((btnRight, modSuper .|. modAlt), (exec "hyprpicker", doNothing))])
          (allPointerBindings defaultConfig)
    , defaultLayouts =
        M.fromList $
          zip
            [1 ..]
            [ myLayout 0
            , myLayout 1
            , myLayout 2
            , myLayout 0
            , myLayout 0
            , myLayout 0
            , myLayout 0
            , myLayout 0
            , myLayout 0
            ]
    , xCursorTheme = ("Himehina", 24)
    , workspaceRules =
        [ ("", "slack", 2)
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
    , windowSizeRules = [("", "beatoraja", 1500, 900)]
    , execOnStart = ["river-tag-overlay"]
    , allKeyBindings =
        M.union
          ( M.fromList
              [ ((keyTab, modSuper), (cycleWindowsOrSlavesOrFocus False))
              , ((keyTab, modSuperShift), (cycleWindowsOrSlavesOrFocus True))
              , ((keyGrave, modSuper), (cycleWindowFocus True))
              , ((keyGrave, modSuperShift), (cycleWindowFocus False))
              , ((keyQ, modSuperShift), (closeAllWindowsOnWorkspace))
              , ((keyW, modSuper), (sendMessage Next))
              , ((keyS, modSuper), (zoomWindow))
              , ((keyEscape, modSuper), (sendMessage ToggleOverview))
              , ((keyR, modSuperShift), (reloadWindowManager (statePath defaultConfig)))
              , ((keyF, modSuperShift), (toggleMaximizeWindow))
              , ((keyEqual, modSuperShift), (sendMessage (IncMasterN 1)))
              , ((keyMinus, modSuperShift), (sendMessage (IncMasterN (-1))))
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
              , ((keyXF86AudioNext, modNone), (exec "mpc next"))
              , ((keyXF86AudioStop, modNone), (exec "mpc stop"))
              , ((keyXF86AudioPlay, modNone), (exec "mpc toggle"))
              , ((keyXF86AudioPrev, modNone), (exec "mpc prev"))
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
  case B.lookup (focusedOutput state) (allOutputWorkspaces state) of
    Nothing -> pure ()
    Just fO -> case layoutName' (workspaceLayouts state M.! fO) of
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
