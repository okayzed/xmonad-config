-- {{{ DESCRIPTION
-- Info on how to use this setup should go here:
-- }}}

-- {{{ IMPORTS

import XMonad hiding ( (|||) )


import XMonad.Actions.CycleRecentWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.TopicSpace
import XMonad.Actions.Commands

import XMonad.Config.Bluetile

-- {{{ HOOKS
import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.Script
import XMonad.Hooks.WorkspaceByPos
-- }}}

-- {{{ LAYOUTS
import XMonad.Layout.BorderResize
import XMonad.Layout.BoringWindows
import XMonad.Layout.Combo
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.NoFrillsDecoration
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.Reflect
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.WindowNavigation
-- }}}

-- {{{ PROMPTS
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad
-- }}}

-- {{{ UTILS
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.Run
import XMonad.Util.Replace
import XMonad.Util.Themes

-- }}}
--
import qualified XMonad.StackSet as W
import qualified Data.Map as M
-- }}}

-- {{{ WINDOW MANAGE HOOKS
myManageHook = composeAll [
  resource =? "desktop_window" --> doIgnore
  , className =? "Guake.py" --> doFloat
  , className =? "synapse" --> doIgnore
  , className =? "Google-Chrome" --> doShift "chrome"
  , className =? "Thunderbird" --> doShift "mail"
  , className =? "Workrave" --> doIgnore
  ]
-- }}}

-- {{{ SCRATCHPADS
scratchpads = [
 -- run htop in xterm, find it by title, use default floating window placement
     NS "htop" "xterm -e htop" (title =? "htop") defaultFloating ,

 -- run gvim, find by role, don't float
     NS "notes" "gvim --role notes ~/TODO" (role =? "notes") nonFloating
  ] where role = stringProperty "WM_WINDOW_ROLE"

-- }}}
-- {{{TOPICS
--
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
  , "vpn"
  , "conf"
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
      , ("music",      spawnApp "chromium-browser --app=https://rdio.com")
      , ("vpn",        spawnApp "vpn_pwd.sh")
      , ("vim",        spawnVimIn "~/")
      , ("todo",       spawnApp "vim TODO")
      , ("web",        browserCmd)
      ]
  }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

mailAction = spawn $ "thunderbird"
browserCmd = spawn $ "chromium-browser"
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

commands :: X [(String, X ())]
commands = defaultCommands
-- }}}

-- {{{ LAYOUT HOOKS
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
      named "TwoPane" subbed |||
      named "Fullscreen" fullscreen |||
      named "Floating" floating
    )
  where
    floating = floatingDeco $ borderResize $ positionStoreFloat
    fullscreen = Full
    tiled1 = mouseResizableTileMirrored {
      draggerType = myDragger
    }
    tiled2 = mouseResizableTile {
      draggerType = myDragger
    }
    subbed = (TwoPane 0.03 0.5)
    floatingDeco l = noFrillsDeco shrinkText defaultTheme l

myDragger = BordersDragger
-- }}}

-- {{{ STARTUP
myStartup = do
  spawn "bash ~/.xinitrc &"
-- }}}

-- {{{ CONFIG
myConfig = bluetileConfig
  { borderWidth = 2
    , normalBorderColor  = "#000" -- "#dddddd"
    , focusedBorderColor = "#999"    -- "#ff0000" don't use hex, not <24 bit safe
    , manageHook = manageHook bluetileConfig <+> myManageHook <+> namedScratchpadManageHook scratchpads
    , focusFollowsMouse  = True
    , layoutHook = bluetileLayoutHook
    , startupHook = ewmhDesktopsStartup <+> myStartup
    , workspaces = myTopics }
    -- {{{ KEYBINDINGS
  `additionalKeys` [
    -- launchers
    ((controlMask, xK_space),    spawn "dmenu-launch")
  , ((controlMask, xK_semicolon), commands >>= runCommand)
  , ((mod4Mask .|. shiftMask, xK_p),    spawn "dmenu_run -b -nb black -nf white")

  -- workspace movement
  , ((mod4Mask, xK_Tab), cycleRecentWS [xK_Super_L] xK_Tab xK_grave)
  , ((mod4Mask, xK_space), dwmpromote)

  -- workspace + window prompts
  , ((mod4Mask .|. shiftMask, xK_BackSpace), removeWorkspace)
  , ((mod4Mask .|. shiftMask, xK_equal   ), addWorkspacePrompt defaultXPConfig)
  , ((mod4Mask .|. shiftMask, xK_r      ), renameWorkspace defaultXPConfig)

  , ((mod4Mask              , xK_n     ), windowPromptGoto defaultXPConfig)
  , ((mod4Mask .|. shiftMask, xK_n     ), windowPromptBring defaultXPConfig)
  , ((mod4Mask              , xK_g     ), promptedGoto)
  , ((mod4Mask .|. shiftMask, xK_g     ), promptedShift)

  -- append to the todo file
  , ((mod4Mask              , xK_y), appendFilePrompt defaultXPConfig "/home/okay/TODO")

  -- switching to different layouts
  , ((mod4Mask              , xK_a), sendMessage $ JumpToLayout "Floating")
  , ((mod4Mask              , xK_s), sendMessage $ JumpToLayout "TwoPane")
  , ((mod4Mask              , xK_d), sendMessage $ JumpToLayout "Tiled")
  , ((mod4Mask              , xK_f), sendMessage $ JumpToLayout "Fullscreen")

  -- running modifiers on layouts
  , ((mod4Mask .|. controlMask, xK_space ), sendMessage $ Toggle NBFULL)
  , ((mod4Mask .|. controlMask, xK_x ), sendMessage $ Toggle REFLECTX)
  , ((mod4Mask .|. controlMask, xK_y ), sendMessage $ Toggle REFLECTY)
  , ((mod4Mask .|. controlMask, xK_m ), sendMessage $ Toggle MIRROR)
  , ((mod4Mask .|. controlMask, xK_b ), sendMessage $ Toggle NOBORDERS)

  ]
  -- }}}
-- }}}

main = xmonad =<< xmobar myConfig

-- vim: foldmethod=marker
