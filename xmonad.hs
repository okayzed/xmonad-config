{-# OPTIONS_GHC -Wno-deprecations #-}
-- vim: foldmethod=marker

-- {{{ DESCRIPTION
-- Info on how to use this setup should go here:
-- }}}

-- {{{ IMPORTS

-- {{{ MISC
import XMonad hiding ((|||))
import XMonad.Config.Bluetile
import XMonad.Hooks.FadeInactive
-- }}}

-- {{{ ACTIONS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.FlexibleManipulate as Flex
import XMonad.Actions.TopicSpace
import XMonad.Actions.Commands
import XMonad.Actions.PhysicalScreens
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.Warp
import XMonad.Actions.OnScreen
-- }}}

-- {{{ HOOKS
import XMonad.Hooks.CurrentWorkspaceOnTop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.DynamicBars
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Minimize
import XMonad.Hooks.PositionStoreHooks
import XMonad.Hooks.Script
import XMonad.Hooks.WorkspaceByPos
import XMonad.Hooks.UrgencyHook
-- }}}

-- {{{ LAYOUTS
import XMonad.Layout.BorderResize
import XMonad.Layout.BoringWindows
import XMonad.Layout.Combo
import XMonad.Layout.DraggingVisualizer
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Grid
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
import XMonad.Layout.SimpleFloat
import XMonad.Layout.SubLayouts
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.WindowSwitcherDecoration
import XMonad.Layout.WindowNavigation

import XMonad.Layout.Monitor
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
import XMonad.Util.Run
import XMonad.Util.Replace
import XMonad.Util.Themes
-- }}}

import qualified XMonad.StackSet as W
import qualified Data.Map as M
import Data.Ratio ((%))
-- }}}

-- {{{ LAYOUTS

myTabConfig = def { decoHeight = 24 }
bluetileLayoutHook =
    mkToggle1 NBFULL $
    mkToggle1 REFLECTX $
    mkToggle1 REFLECTY $
    mkToggle1 NOBORDERS $
    avoidStrutsOn [U] $
    minimize $
    boringWindows $ lessBorders Screen (
      named "Fullscreen" fullscreen |||
      named "Tiled" tiled2 |||
      named "TwoPane" subbed |||
      named "Floating" floating |||
      named "Grid" grid
    )
  where
    floating = floatingDeco $ borderResize $ positionStoreFloat
    fullscreen = Full
    tiled2 = mouseResizableTile { draggerType = myDragger }
    subbed = TwoPane 0.03 0.5
    grid = Grid
    floatingDeco _l = floatSimpleSimple

myDragger = BordersDragger
-- }}}

-- {{{ TOPICS / WORKSPACES

-- NOTE: expanded to include:
-- left-pinned:  1..7
-- right-pinned: 8,9,0,-,=,\
-- flexible:     u,i,o,p,z,x,c,v
myTopics :: [Topic]
myTopics =
  [ "1","2","3","4","5","6","7"
  , "8","9","0","-","=","\\"
  , "u","i","o","p","z","x","c","v"
  ]

skipTopics :: [Topic]
skipTopics = []

goto :: Topic -> X ()
goto = switchTopic def

promptedGoto :: X ()
promptedGoto = workspacePrompt def goto

promptedShift :: X ()
promptedShift = workspacePrompt def $ windows . W.shift

commands :: X [(String, X ())]
commands = def

-- Screen helpers (physical order: left-to-right)
screenOrder :: ScreenComparator
screenOrder = horizontalScreenOrderer

leftScreen, rightScreen :: PhysicalScreen
leftScreen  = 0
rightScreen = 1

-- Pinned topics update a *specific* monitor without changing keyboard focus.
gotoPinned :: PhysicalScreen -> WorkspaceId -> X ()
gotoPinned ps ws = do
  msid <- getScreen screenOrder ps
  case msid of
    Just sid -> windows (viewOnScreen sid ws)

-- Mod-h / Mod-l: focus monitor AND warp pointer there (only here do we move the mouse).
warpToPhysical :: PhysicalScreen -> X ()
warpToPhysical ps = do
  msid <- getScreen screenOrder ps
  case msid of
    Nothing  -> pure ()
    Just sid -> warpToScreen sid (1%2) (1%2)

focusAndWarp :: PhysicalScreen -> X ()
focusAndWarp ps = viewScreen screenOrder ps >> warpToPhysical ps
-- }}}

-- {{{ WINDOW MANAGE HOOKS
myManageHook = composeAll
  [ resource =? "desktop_window" --> doIgnore
  , resource =? "Desktop" --> doFullFloat
  , className =? "synapse" --> doIgnore
  , className =? "Docky" --> doIgnore
  , className =? "Workrave" --> doIgnore
  , className =? "onboard" --> doIgnore
  , className =? "xfce4-panel" --> doIgnore
  ]
-- }}}

-- {{{ KEYBINDINGS

modm = mod4Mask

-- Topic/key groups
leftPinned :: [(WorkspaceId, KeySym)]
leftPinned =
  zip ["1","2","3","4","5","6","7"] [xK_1, xK_2, xK_3, xK_4, xK_5, xK_6, xK_7]

rightPinned :: [(WorkspaceId, KeySym)]
rightPinned =
  [ ("8", xK_8)
  , ("9", xK_9)
  , ("0", xK_0)
  , ("-", xK_minus)
  , ("=", xK_equal)
  , ("\\", xK_backslash)
  ]

flexTopics :: [(WorkspaceId, KeySym)]
flexTopics =
  [ ("u", xK_u)
  , ("i", xK_i)
  , ("o", xK_o)
  , ("p", xK_p)
  , ("z", xK_z)
  , ("x", xK_x)
  , ("c", xK_c)
  , ("v", xK_v)
  ]

myAdditionalKeys :: [((KeyMask, KeySym), X ())]
myAdditionalKeys =
  [
    ((controlMask , xK_semicolon), spawn "dmenu_run -b -nb black -nf white")
  , ((controlMask , xK_space),    spawn "/usr/bin/rofi -combi-modi drun,run -show combi -modi combi")
  , ((controlMask .|. shiftMask, xK_semicolon), spawn "dashdoc")
  , ((modm , xK_Return), currentTopicAction def)

  -- workspace movement history
  , ((modm, xK_Tab), cycleRecentWS [xK_Super_L] xK_Tab xK_grave)
  , ((modm, xK_space), dwmpromote)

  -- workspace + window prompts
  , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
  , ((modm .|. shiftMask, xK_equal   ), addWorkspacePrompt def)
  , ((modm .|. shiftMask, xK_r      ), renameWorkspace def)

  -- append to the todo file
  , ((controlMask .|. shiftMask , xK_y), appendFilePrompt def "/home/okay/TODO")
  , ((controlMask .|. shiftMask , xK_j), appendFilePrompt def "/home/okay/SJRN")

  -- brightness
  , ((0, xK_F5), spawn "xbacklight -dec 2")
  , ((0, xK_F6), spawn "xbacklight -inc 2")

  -- switching to different layouts
  -- NOTE: removed mod-a and mod-s per request; leaving d and f.
  , ((modm, xK_d), sendMessage $ JumpToLayout "Tiled")
  , ((modm, xK_f), sendMessage $ JumpToLayout "Fullscreen")
  , ((modm, xK_g), sendMessage $ JumpToLayout "Grid")
  , ((modm, xK_r), sendMessage NextLayout)
  , ((modm .|. shiftMask, xK_b), sendMessage $ ToggleStrut D)
  , ((modm .|. shiftMask, xK_u), sendMessage $ ToggleStrut U)

  -- running modifiers on layouts
  , ((modm .|. controlMask, xK_space), sendMessage $ Toggle NBFULL)
  , ((modm .|. controlMask, xK_x), sendMessage $ Toggle REFLECTX)
  , ((modm .|. controlMask, xK_y), sendMessage $ Toggle REFLECTY)
  , ((modm .|. controlMask, xK_b), sendMessage $ Toggle NOBORDERS)

  -- monitor mouse movement (replaces q/w/e idea): mod-h/l
  , ((modm, xK_h), focusAndWarp leftScreen)
  , ((modm, xK_l), focusAndWarp rightScreen)

  -- move windows between monitors: shift-mod-h/l
  , ((modm .|. shiftMask, xK_h), sendToScreen screenOrder leftScreen)
  , ((modm .|. shiftMask, xK_l), sendToScreen screenOrder rightScreen)
  ]
  ++
  -- LEFT PINNED TOPICS: mod or ctrl to view; mod/ctrl + shift to move window
  [ ((m, k), gotoPinned leftScreen ws)
      | (ws, k) <- leftPinned
      , m <- [modm, controlMask]
  ]
  ++
  [ ((m .|. shiftMask, k), windows (W.shift ws))
      | (ws, k) <- leftPinned
      , m <- [modm, controlMask]
  ]
  ++
  -- RIGHT PINNED TOPICS: mod or ctrl to view; mod/ctrl + shift to move window
  [ ((m, k), gotoPinned rightScreen ws)
      | (ws, k) <- rightPinned
      , m <- [modm, controlMask]
  ]
  ++
  [ ((m .|. shiftMask, k), windows (W.shift ws))
      | (ws, k) <- rightPinned
      , m <- [modm, controlMask]
  ]
  ++
  -- FLEX TOPICS: mod only to view on *current* monitor; mod-shift to move window
  [ ((modm, k), goto ws)
      | (ws, k) <- flexTopics
  ]
  ++
  [ ((modm .|. shiftMask, k), windows (W.shift ws))
      | (ws, k) <- flexTopics
  ]

-- }}}

-- {{{ MOUSE BINDINGS (unchanged)
myMouseMoveWindow :: Window -> X ()
myMouseMoveWindow w = whenX (isClient w) $ withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    (_, _, _, ox', oy', _, _, _) <- io $ queryPointer d w
    let ox = fromIntegral ox'
        oy = fromIntegral oy'
    mouseDrag (\ex ey -> io $ moveWindow d w (fromIntegral (fromIntegral (wa_x wa) + (ex - ox)))
                                             (fromIntegral (fromIntegral (wa_y wa) + (ey - oy))))
      (focus w)

myMouse x  =
  [ ((modm, button1), (\w -> focus w >> myMouseMoveWindow w))
  , ((modm, button3), (\w -> focus w >> mouseResizeWindow w))
  ]

newMouse x = M.union (mouseBindings def x) (M.fromList (myMouse x))
-- }}}

-- {{{ STARTUP
myStartup = do
  spawn "bash ~/.xinitrc &"
-- }}}

-- {{{ CONFIG

barCreator :: DynamicStatusBar
barCreator (XMonad.S sid) = spawnPipe $ "xmobar --screen " ++ show sid

barDestroyer :: DynamicStatusBarCleanup
barDestroyer = return ()

myConfig =
  withUrgencyHook NoUrgencyHook $ bluetileConfig
    { borderWidth        = 2
    , normalBorderColor  = "#000"
    , focusedBorderColor = "#999"
    , manageHook         = manageHook bluetileConfig <+> myManageHook
    , focusFollowsMouse  = True
    , layoutHook         = noBorders bluetileLayoutHook
    , startupHook        = ewmhDesktopsStartup <+> myStartup <+> dynStatusBarStartup barCreator barDestroyer

    , logHook            = ewmhDesktopsLogHook <+> dynamicLogXinerama <+> multiPP myPP myPP

    , modMask            = modm
    , mouseBindings      = newMouse
    , workspaces         = myTopics
    }
    `additionalKeys` myAdditionalKeys
-- }}}

-- {{{ MAIN
myBar = "xmobar"

hideStr :: String -> String
hideStr _ = ""

myPP = xmobarPP
  { ppCurrent = xmobarColor "#af87d7" ""
  , ppTitle   = xmobarColor "#afd700"  "" . shorten 40
  , ppVisible = xmobarColor "#ff5faf" ""
  , ppLayout  = hideStr
  , ppSep     = " | "
  , ppUrgent  = xmobarColor "" "#ffcc33"
  }

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
-- }}}

