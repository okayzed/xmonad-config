import XMonad hiding ( (|||) )

import XMonad.Config.Bluetile
import XMonad.Config.Gnome
import XMonad.Util.Replace
import XMonad.Util.Themes
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Hooks.FadeInactive



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

import XMonad.Layout.Cross
import XMonad.Layout.Circle

import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.Minimize
import XMonad.Hooks.ServerMode
import XMonad.Hooks.WorkspaceByPos
import XMonad.Layout.DwmStyle

myManageHook = composeAll [
  resource =? "desktop_window" --> doIgnore
  , className =? "synapse" --> doIgnore
  ]

myDragger = BordersDragger


bluetileLayoutHook = avoidStruts $ minimize $ boringWindows $ (
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

myLogHook = fadeInactiveLogHook fadeAmount
      where fadeAmount = 0.9


main = xmonad $ bluetileConfig
  { borderWidth = 2
    , normalBorderColor  = "#000" -- "#dddddd"
    , focusedBorderColor = "#999"    -- "#ff0000" don't use hex, not <24 bit safe
    , manageHook = manageHook bluetileConfig <+> myManageHook
    , focusFollowsMouse  = True
    , layoutHook = bluetileLayoutHook
    , logHook = myLogHook
  } `additionalKeys`
  [ ((mod4Mask, xK_p),    spawn "dmenu_run -b -nb black -nf white") --call dmenu
  ]
