module Config (myConfig) where

import Control.Concurrent.MVar
import Data.Bimap qualified as B
import Data.List
import Data.Map.Strict qualified as M
import Foreign
import Layouts.Basic
import Layouts.Magnifier
import Layouts.Overview
import Types
import Utils.DefaultConfig
import Utils.KeyDispatches
import Utils.Keysyms

myConfig :: RivermonadConfig
myConfig =
  defaultConfig
    { allPointerBindings =
        M.union
          (M.fromList [((BtnRight, modSuperAlt), (exec "hyprpicker", doNothing))])
          (allPointerBindings defaultConfig)
    , defaultLayouts =
        M.fromList $
          zip
            [1 ..]
            ( overview False
                <$> [ choose 0 [monocle, twoPane 0.6]
                    , magnifierNum' 1.5 (tall 0.6 1) 2
                    , choose 0 [monocle, twoPane 0.6]
                    , choose 0 [monocle, magnifierNum' 1.5 (threeCol 0.5) 4]
                    , choose 0 [twoPane 0.6, magnifierNum' 1.5 (threeCol 0.5) 4]
                    , choose 0 [monocle, twoPane 0.6, magnifierNum' 1.5 (tall 0.6 1) 2, magnifierNum' 1.5 (threeCol 0.5) 4]
                    , choose 0 [monocle, twoPane 0.6, magnifierNum' 1.5 (tall 0.6 1) 2, magnifierNum' 1.5 (threeCol 0.5) 4]
                    , choose 0 [monocle, twoPane 0.6, magnifierNum' 1.5 (tall 0.6 1) 2, magnifierNum' 1.5 (threeCol 0.5) 4]
                    , choose 0 [monocle, twoPane 0.6, magnifierNum' 1.5 (tall 0.6 1) 2, magnifierNum' 1.5 (threeCol 0.5) 4]
                    ]
            )
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
              [ ((KeyTab, ModSuper), (cycleWindowsOrSlavesOrFocus False))
              , ((KeyTab, modSuperShift), (cycleWindowsOrSlavesOrFocus True))
              , ((KeyGrave, ModSuper), (sendMessage NextLayout))
              , ((KeyGrave, modSuperShift), (sendMessage FirstLayout))
              , ((KeyW, ModSuper), (sendMessage ToggleMagnifier))
              , ((KeyQ, modSuperShift), (closeAllWindowsOnWorkspace))
              , ((KeyS, ModSuper), (zoomWindow))
              , ((KeyEscape, ModSuper), (sendMessage ToggleOverview))
              , ((KeyR, modSuperShift), (reloadWindowManager (statePath defaultConfig)))
              , ((KeyF, modSuperShift), (toggleMaximizeWindow))
              , ((KeyEqual, modSuperShift), (sendMessage (IncMasterN 1)))
              , ((KeyMinus, modSuperShift), (sendMessage (IncMasterN (-1))))
              , ((KeyEnter, ModSuper), (exec "foot"))
              , ((KeyZ, ModSuper), (exec "foot -e yazi"))
              , ((KeyX, ModSuper), (exec "foot -e nvim"))
              , ((KeyV, ModSuper), (exec "foot -e calpersonal"))
              , ((KeyB, ModSuper), (exec "foot -e btop"))
              , ((KeyN, ModSuper), (exec "foot -e ncmpcpp"))
              , ((KeyM, ModSuper), (exec "foot -e neomutt"))
              , ((KeyA, ModSuper), (exec "firefox"))
              , ((KeyD, ModSuper), (exec "~/.config/rofi/launcher/launcher.sh"))
              , ((KeyE, ModSuper), (exec "~/.config/rofi/notification/notification.sh"))
              , ((KeyO, ModSuper), (exec "~/.config/rofi/password/password.sh"))
              , ((KeyI, ModSuper), (exec "~/.config/rofi/mirror/mirror.sh"))
              , ((KeyC, ModSuper), (exec "~/.config/rofi/powermenu/powermenu.sh"))
              , ((KeyU, ModSuper), (exec "screenrecorder toggle fullscreen"))
              , ((KeyU, modSuperShift), (exec "screenrecorder toggle region"))
              , ((KeyXF86Calculator, ModSuper), (exec "~/.config/river/screenshot fullscreen"))
              , ((KeyXF86Calculator, modSuperShift), (exec "~/.config/river/screenshot region"))
              , ((KeyXF86AudioNext, ModNone), (exec "mpc next"))
              , ((KeyXF86AudioStop, ModNone), (exec "mpc stop"))
              , ((KeyXF86AudioPlay, ModNone), (exec "mpc toggle"))
              , ((KeyXF86AudioPrev, ModNone), (exec "mpc prev"))
              ]
          )
          (allKeyBindings defaultConfig)
    , keyboardOptions =
        HsXkbRuleNames
          { hsXkbRules = Nothing
          , hsXkbModel = Nothing
          , hsXkbLayout = Nothing
          , hsXkbVariant = Nothing
          , hsXkbOptions = Just "compose:rctrl"
          }
    }

cycleWindowsOrSlaves :: Bool -> Ptr RiverSeat -> MVar WMState -> IO ()
cycleWindowsOrSlaves forward seat stateMVar = do
  state <- readMVar stateMVar
  case B.lookup (focusedOutput state) (allOutputWorkspaces state) of
    Nothing -> pure ()
    Just fO ->
      if "TwoPane" `isInfixOf` (layoutName' (workspaceLayouts state M.! fO))
        then cycleWindowSlaves forward seat stateMVar
        else cycleWindows forward seat stateMVar

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
