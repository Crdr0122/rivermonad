module Config (myConfig) where

import Control.Concurrent.MVar
import Control.Monad.IO.Class
import Data.Bimap qualified as B
import Data.List
import Data.Map.Strict qualified as M
import Layouts.Basic
import Layouts.Magnifier
import Layouts.Overview
import Protocols.Generated
import Types
import Utils.DefaultConfig
import Utils.Helpers
import Utils.KeyDispatches
import Utils.Keysyms
import Wayland.Connection

myConfig :: RivermonadConfig
myConfig =
  defaultConfig
    { allPointerBindings =
        M.union
          (M.fromList [((BtnRight, modSuperAlt), (exec "hyprpicker", doNothing))])
          (allPointerBindings defaultConfig)
    , xCursorTheme = ("Himehina", 24)
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
              [ ((KeyTab, modSuper), (cycleWindowsOrSlavesOrFocus False))
              , ((KeyTab, modSuperShift), (cycleWindowsOrSlavesOrFocus True))
              , ((KeyGrave, modSuper), (sendMessage NextLayout))
              , ((KeyGrave, modSuperShift), (sendMessage FirstLayout))
              , ((KeyW, modSuper), (sendMessage ToggleMagnifier))
              , ((KeyQ, modSuperShift), (closeAllWindowsOnWorkspace))
              , ((KeyS, modSuper), (zoomWindow))
              , ((KeyEscape, modSuper), (sendMessage ToggleOverview))
              , ((KeyR, modSuperShift), (reloadWindowManager (statePath defaultConfig)))
              , ((KeyF, modSuperShift), (toggleMaximizeWindow))
              , ((KeyEqual, modSuperShift), (sendMessage (IncMasterN 1)))
              , ((KeyMinus, modSuperShift), (sendMessage (IncMasterN (-1))))
              , ((KeyEnter, modSuper), (exec "foot"))
              , ((KeyZ, modSuper), (exec "foot -e yazi"))
              , ((KeyX, modSuper), (exec "foot -e nvim"))
              , ((KeyV, modSuper), (exec "foot -e calpersonal"))
              , ((KeyB, modSuper), (exec "foot -e btop"))
              , ((KeyN, modSuper), (exec "foot -e ncmpcpp"))
              , ((KeyM, modSuper), (exec "foot -e neomutt"))
              , ((KeyA, modSuper), (exec "firefox"))
              , ((KeyD, modSuper), (exec "~/.config/rofi/launcher/launcher.sh"))
              , ((KeyE, modSuper), (exec "~/.config/rofi/notification/notification.sh"))
              , ((KeyO, modSuper), (exec "~/.config/rofi/password/password.sh"))
              , ((KeyI, modSuper), (exec "~/.config/rofi/mirror/mirror.sh"))
              , ((KeyC, modSuper), (exec "~/.config/rofi/powermenu/powermenu.sh"))
              , ((KeyU, modSuper), (exec "screenrecorder toggle fullscreen"))
              , ((KeyU, modSuperShift), (exec "screenrecorder toggle region"))
              , ((KeyXF86Calculator, modSuper), (exec "~/.config/river/screenshot fullscreen"))
              , ((KeyXF86Calculator, modSuperShift), (exec "~/.config/river/screenshot region"))
              , ((KeyXF86AudioNext, modNone), (exec "mpc next"))
              , ((KeyXF86AudioStop, modNone), (exec "mpc stop"))
              , ((KeyXF86AudioPlay, modNone), (exec "mpc toggle"))
              , ((KeyXF86AudioPrev, modNone), (exec "mpc prev"))
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
    , keyboardRepeatInfo = Nothing
    }

cycleWindowsOrSlaves :: Bool -> Object RiverSeatV1 -> MVar WMState -> W ()
cycleWindowsOrSlaves forward seat stateMVar = do
  state <- liftIO $ readMVar stateMVar
  case B.lookup (focusedOut state) (allOutputWorkspaces state) of
    Nothing -> pure ()
    Just fO ->
      if "TwoPane" `isInfixOf` (layoutName' (workspaceLayouts state M.! fO))
        then cycleWindowSlaves forward seat stateMVar
        else cycleWindows forward seat stateMVar

cycleWindowsOrSlavesOrFocus :: Bool -> Object RiverSeatV1 -> MVar WMState -> W ()
cycleWindowsOrSlavesOrFocus forward seat stateMVar = do
  state <- liftIO $ readMVar stateMVar
  case focusedWin state of
    Nothing -> pure ()
    Just w -> do
      let Window{winFloat, winFull} = (allWindows state M.! w)
      if winFloat || winFull
        then cycleWindowFocus forward seat stateMVar
        else cycleWindowsOrSlaves forward seat stateMVar
