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
import XMonad.Layout.Decoration
import XMonad.Layout.DecorationAddons
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.Maximize
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.WindowSwitcherDecoration

import XMonad.Prompt
import XMonad.Prompt.Shell
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace
import XMonad.Prompt.XMonad

import XMonad.Util.EZConfig
import XMonad.Util.Replace
import XMonad.Util.Themes

import qualified XMonad.StackSet as W
import qualified Data.Map as M



myManageHook = composeAll [
  resource =? "desktop_window" --> doIgnore
  , className =? "synapse" --> doIgnore
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
  , "chrome"
  , "mail"
  , "vim"
  , "todo"
  , "top"
  ]

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
  { topicDirs = M.fromList $
      [ ("conf", "w/conf")
      , ("music", "Music")
      , ("terminal", "w/tonka/src")
      ]
  , defaultTopicAction = const $ spawnShell >*> 3
  , defaultTopic = "dashboard"
  , topicActions = M.fromList $
      [ ("conf",       spawnShell >> spawnShellIn ".xmonad")
      , ("terminal",   spawnShellIn "wd" >>
                       spawnShellIn "wd" >>
                       spawnShellIn "wd")
      , ("mail",       mailAction)
      , ("irc",        spawnShell)
      , ("vim",        spawnVimIn "wd")
      , ("todo",       spawnVimIn "wd")
      , ("dashboard",  spawnShell)
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
spawnShellIn dir = spawn $ "urxvt '(cd ''" ++ dir ++ "'' && " ++ myShell ++ " )'"

spawnVimIn :: Dir -> X ()
spawnVimIn dir = spawn $ "urxvt '(cd ''" ++ dir ++ "'' && " ++ myShell ++ " && vim)'"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt defaultXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt defaultXPConfig $ windows . W.shift

bluetileLayoutHook = avoidStruts $ minimize $ boringWindows $ (
    named "Dashboard" tiled2 |||
    named "Floating" floating |||
    named "Tiled1" tiled1 |||
    named "Tiled2" tiled2 |||
    named "Fullscreen" fullscreen
    )
  where
    floating = maximize $ borderResize $ positionStoreFloat
    tiled1 = maximize $ mouseResizableTileMirrored {
      draggerType = myDragger
    }
    tiled2 = maximize $ mouseResizableTile {
      draggerType = myDragger
    }
    fullscreen = maximize $ smartBorders Full

myStartup = do
  spawn "bash ~/.xinitrc"

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
    ((mod4Mask, xK_p),    spawn "dmenu_run -b -nb black -nf white") --call dmenu
  , ((mod4Mask, xK_o), moveTo Prev NonEmptyWS)
  , ((mod4Mask, xK_i), moveTo Next NonEmptyWS)
  , ((mod4Mask, xK_space), dwmpromote)
  , ((mod4Mask              , xK_n     ), windowPromptGoto defaultXPConfig)
  , ((mod4Mask .|. shiftMask, xK_n     ), windowPromptBring defaultXPConfig)
  , ((mod4Mask              , xK_g     ), promptedGoto)
  , ((mod4Mask .|. shiftMask, xK_g     ), promptedShift)
  ]

main = xmonad =<< xmobar myConfig
