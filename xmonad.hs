import XMonad hiding ( (|||) )

import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.TopicSpace


import XMonad.Config.Bluetile
import XMonad.Config.Gnome

import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.Script
import XMonad.Hooks.ServerMode
import XMonad.Hooks.WorkspaceByPos

import XMonad.Layout.BorderResize
import XMonad.Layout.BoringWindows
import XMonad.Layout.ButtonDecoration
import XMonad.Layout.Column
import XMonad.Layout.Combo
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.WindowNavigation

import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.Replace
import XMonad.Util.Themes

import qualified XMonad.StackSet as W
import qualified Data.Map as M



myManageHook = composeAll [
  resource =? "desktop_window" --> doIgnore
  , className =? "Guake.py" --> doFloat
  , className =? "synapse" --> doIgnore
  , className =? "Google-Chrome" --> doShift "chrome"
  ]
myDragger = BordersDragger

-- The list of all topics/workspaces of your xmonad configuration.
-- The order is important, new topics must be inserted
-- at the end of the list if you want hot-restarting
-- to work.
myTopics :: [Topic]
myTopics =
  [ "web" -- the first one
  , "music"
  , "todo"
  , "mail"
  , "terminal"
  , "vim"
  , "ssh"
  , "conf"
  , "top"
  , "chrome"
  ]

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
  { topicDirs = M.fromList $
      [ ("conf", ".xmonad/")
      , ("music", "Music")
      , ("wiki", "wiki")
      , ("terminal", "tonka/src")
      ]
  , defaultTopicAction = const $ spawnShell >*> 3
  , defaultTopic = "dashboard"
  , topicActions = M.fromList $
      [ ("conf",       spawnApp "vim .xmonad/xmonad.hs" >>
                       spawnShell)
      , ("top",        spawnApp "iftop -i wlan1" >>
                       spawnApp "iftop -i eth1" >>
                       spawnApp "top"
                       )
      , ("terminal",   spawnShellIn "~/" >>
                       spawnShellIn "~/" >>
                       spawnShellIn "~/")
      , ("mail",       mailAction)
      , ("ssh",        spawnShell)
      , ("vim",        spawnVimIn "~/")
      , ("todo",       spawnApp "vim TODO")
      , ("web",        browserCmd)
      ]
  }

-- extend your keybindings
spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

mailAction = spawn $ "thunderbird"
browserCmd = spawn $ "xdg-open http://google.com"
myShell = "bash"

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ "gnome-terminal --working-directory=" ++ dir ++ " -e '" ++ myShell ++ "'"

spawnApp :: String -> X ()
spawnApp app = spawn $ "gnome-terminal -e '" ++ app ++ "'"

spawnVimIn :: Dir -> X ()
spawnVimIn dir = spawn $ "gnome-terminal --working-directory=" ++ dir ++ " -e vim"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt defaultXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt defaultXPConfig $ windows . W.shift

bluetileLayoutHook =
    mkToggle1 NBFULL $
    mkToggle1 REFLECTX $
    mkToggle1 REFLECTY $
    mkToggle1 NOBORDERS $
    mkToggle1 MIRROR $
    avoidStruts $
    minimize $
    boringWindows $ (
      named "Tiled" tiled2 |||
      named "Tabbed" tabbed |||
      named "Floating" floating |||
      named "Fullscreen" fullscreen
    )
  where
    floating = maximize $ borderResize $ positionStoreFloat
    fullscreen = maximize $ smartBorders Full
    tiled1 = maximize $ mouseResizableTileMirrored {
      draggerType = myDragger
    }
    tiled2 = maximize $ mouseResizableTile {
      draggerType = myDragger
    }
    tabbed = maximize $ tabbedBottomAlways shrinkText defaultTheme

myStartup = do
  spawn "bash ~/.xinitrc &"

myConfig = bluetileConfig
  { borderWidth = 2
    , normalBorderColor  = "#000" -- "#dddddd"
    , focusedBorderColor = "#999"    -- "#ff0000" don't use hex, not <24 bit safe
    , manageHook = manageHook gnomeConfig <+> myManageHook
    , focusFollowsMouse  = True
    , layoutHook = bluetileLayoutHook
    , startupHook = ewmhDesktopsStartup <+> myStartup
    , workspaces = myTopics }
  `additionalKeys` [
    ((controlMask, xK_space),    spawn "DMENU_OPTIONS='-b -nb black -nf white' dmenu-launch")
  , ((mod4Mask .|. shiftMask, xK_p),    spawn "dmenu_run -b -nb black -nf white")
    --call dmenu

  -- workspace movement
  , ((mod4Mask, xK_o), moveTo Prev NonEmptyWS)
  , ((mod4Mask, xK_i), moveTo Next NonEmptyWS)
  , ((mod4Mask, xK_space), dwmpromote)

  -- prompts
  , ((mod4Mask              , xK_n     ), windowPromptGoto defaultXPConfig)
  , ((mod4Mask .|. shiftMask, xK_n     ), windowPromptBring defaultXPConfig)
  , ((mod4Mask              , xK_g     ), promptedGoto)
  , ((mod4Mask .|. shiftMask, xK_g     ), promptedShift)

  -- append to the todo file
  , ((mod4Mask              , xK_y), appendFilePrompt defaultXPConfig "/home/okay/TODO")

  -- switching to different layouts
  , ((mod4Mask              , xK_a), sendMessage $ JumpToLayout "Floating")
  , ((mod4Mask              , xK_s), sendMessage $ JumpToLayout "Tabbed")
  , ((mod4Mask              , xK_d), sendMessage $ JumpToLayout "Tiled")
  , ((mod4Mask              , xK_f), sendMessage $ JumpToLayout "Fullscreen")

  -- running modifiers on layouts
  , ((mod4Mask .|. controlMask, xK_space ), sendMessage $ Toggle NBFULL)
  , ((mod4Mask .|. controlMask, xK_x ), sendMessage $ Toggle REFLECTX)
  , ((mod4Mask .|. controlMask, xK_y ), sendMessage $ Toggle REFLECTY)
  , ((mod4Mask .|. controlMask, xK_m ), sendMessage $ Toggle MIRROR)
  , ((mod4Mask .|. controlMask, xK_b ), sendMessage $ Toggle NOBORDERS)
  ]

main = xmonad =<< xmobar myConfig
