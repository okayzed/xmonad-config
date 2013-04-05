-- {{{ DESCRIPTION
-- Info on how to use this setup should go here:
-- }}}

-- {{{ IMPORTS

-- {{{ MISC
import XMonad hiding ( (|||) )
import XMonad.Config.Bluetile
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
-- }}}


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
import XMonad.Layout.DecorationMadness
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

-- {{{ LAYOUTS
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
    fullscreen = simpleTabbed
    tiled1 = mouseResizableTileMirrored {
      draggerType = myDragger
    }
    tiled2 = mouseResizableTile {
      draggerType = myDragger
    }
    subbed = (TwoPane 0.03 0.5)
    floatingDeco l = floatSimpleSimple

myDragger = BordersDragger
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
  , "ssh"
  , "mail"
  , "gimp"
  , "skype"
  , "scratch"
  , "vpn"
  , "top"
  , "maps"
  , "conf"
  ]

skipTopics :: [Topic]
skipTopics = []

myTopicConfig :: TopicConfig
myTopicConfig = defaultTopicConfig
  { topicDirs = M.fromList $
      [ ("conf", ".xmonad/")
      , ("music", "Music")
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
      , ("todo",       spawnApp "vim note:TODO")
      , ("maps",       spawnApp "chromium-browser --app=https://maps.google.com")
      , ("web",        browserCmd)
      ]
  }

spawnShell :: X ()
spawnShell = currentTopicDir myTopicConfig >>= spawnShellIn

mailAction = spawn $ "thunderbird"
browserCmd = spawn $ "chromium-browser"
myShell = "bash"
myTerm = "gnome-terminal "

spawnShellIn :: Dir -> X ()
spawnShellIn dir = spawn $ myTerm ++ "--working-directory=" ++ dir ++ " -e '" ++ myShell ++ "'"

spawnApp :: String -> X ()
spawnApp app = spawn $ myTerm ++ "-e '" ++ app ++ "'"

spawnVimIn :: Dir -> X ()
spawnVimIn dir = spawn $ myTerm ++ "--working-directory=" ++ dir ++ " -e vim"

goto :: Topic -> X ()
goto = switchTopic myTopicConfig

promptedGoto :: X ()
promptedGoto = workspacePrompt defaultXPConfig goto

promptedShift :: X ()
promptedShift = workspacePrompt defaultXPConfig $ windows . W.shift

commands :: X [(String, X ())]
commands = defaultCommands
-- }}}

-- {{{ SCRATCHPADS
scratchpads = [
     -- run htop in xterm, find it by title, use default floating window placement
     NS "htop" "xterm -e htop" (title =? "htop") defaultFloating
     , NS "term"  (myTerm ++ " --role term") (role =? "term") manageTopTerm
     , NS "term2"  (myTerm ++ " --role term2") (role =? "term2") manageTerm

     -- run gvim, find by role, don't float
     , NS "notes" "gvim --role notes note:" (role =? "notes") manageNote
     , NS "todo" "gvim --role todo note:TODO" (role =? "todo") manageNote
  ] where
    role = stringProperty "WM_WINDOW_ROLE"
    manageNote = (customFloating $ W.RationalRect l 0.1 w 0.85)
    manageTopTerm = (customFloating $ W.RationalRect l 0 w h)
    manageTerm = (customFloating $ W.RationalRect l t w h)
    -- where clauses
    h = 0.5       -- height, 50%
    w = 1         -- width, 100%
    t = 1 - h     -- bottom edge
    l = (1 - w)/2 -- centered left/right

-- }}}

-- {{{ WINDOW MANAGE HOOKS
myManageHook = composeAll [
  resource =? "desktop_window" --> doIgnore
  , className =? "synapse" --> doIgnore
  , className =? "Google-Chrome" --> doShift "chrome"
  , className =? "Thunderbird" --> doShift "mail"
  , className =? "Workrave" --> doIgnore
  ]
-- }}}

-- {{{ KEYBINDINGS
myAdditionalKeys = [
  ((controlMask, xK_semicolon), commands >>= runCommand)
  -- launchers
  , ((modm .|. shiftMask, xK_p),    spawn "dmenu_run -b -nb black -nf white")
  , ((modm              , xK_Return), currentTopicAction myTopicConfig)

  -- workspace movement
  , ((modm, xK_Tab), cycleRecentWS [xK_Super_L] xK_Tab xK_grave)
  , ((modm, xK_space), dwmpromote)

  , ((modm .|. shiftMask, xK_h), swapTo Next)
  , ((modm .|. shiftMask, xK_l), swapTo Prev)
  -- workspace + window prompts
  , ((modm .|. shiftMask, xK_BackSpace), removeWorkspace)
  , ((modm .|. shiftMask, xK_equal   ), addWorkspacePrompt defaultXPConfig)
  , ((modm .|. shiftMask, xK_r      ), renameWorkspace defaultXPConfig)

  , ((modm              , xK_n     ), windowPromptGoto defaultXPConfig)
  , ((modm .|. shiftMask, xK_n     ), windowPromptBring defaultXPConfig)
  , ((modm              , xK_g     ), promptedGoto)
  , ((modm .|. shiftMask, xK_g     ), promptedShift)

  -- append to the todo file
  , ((modm              , xK_y), appendFilePrompt defaultXPConfig "/home/okay/TODO")

  -- switching to different layouts
  , ((modm              , xK_a), sendMessage $ JumpToLayout "Floating")
  , ((modm              , xK_s), sendMessage $ JumpToLayout "TwoPane")
  , ((modm              , xK_d), sendMessage $ JumpToLayout "Tiled")
  , ((modm              , xK_f), sendMessage $ JumpToLayout "Fullscreen")

  -- running modifiers on layouts
  , ((modm .|. controlMask, xK_space ), sendMessage $ Toggle NBFULL)
  , ((modm .|. controlMask, xK_x ), sendMessage $ Toggle REFLECTX)
  , ((modm .|. controlMask, xK_y ), sendMessage $ Toggle REFLECTY)
  , ((modm .|. controlMask, xK_m ), sendMessage $ Toggle MIRROR)
  , ((modm .|. controlMask, xK_b ), sendMessage $ Toggle NOBORDERS)

  -- scratchpads
  , ((shiftMask, xK_F1), namedScratchpadAction scratchpads "term")
  , ((shiftMask, xK_F12), namedScratchpadAction scratchpads "term2")
  , ((controlMask, xK_Insert), namedScratchpadAction scratchpads "notes")
  , ((controlMask, xK_Home), namedScratchpadAction scratchpads "todo")
  ]
  ++
  -- mod-[1..9] ++ [0] %! Switch to workspace N
  -- mod-shift-[1..9] ++ [0] %! Move client to workspace N
  [((m .|. modm, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces myConfig) ([xK_1 .. xK_9] ++ [xK_0])
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


  -- physical screens
  --
  -- mod-{[,],\}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{[,],\}, Move client to screen 1, 2, or 3
  --
  ++
  [((modm .|. mask, key), f sc)
    | (key, sc) <- zip [xK_bracketleft, xK_bracketright, xK_backslash] [0..]
    , (f, mask) <- [(viewScreen, 0), (sendToScreen, shiftMask)]]


-- }}}

-- {{{ MOUSE BINDINGS
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

myMouse x  = [
  -- optional. but nicer than normal mouse move and size
  ((modm, button1), (\w -> focus w >> myMouseMoveWindow w))
  , ((modm, button3), (\w -> focus w >> mouseResizeWindow w))
  ]


newMouse x = M.union (mouseBindings defaultConfig x) (M.fromList (myMouse x))
-- }}}

-- {{{ STARTUP
myStartup = do
  spawn "bash ~/.xinitrc &"
-- }}}

-- {{{ CONFIG
modm = mod1Mask
myConfig = bluetileConfig
  { borderWidth = 2
    , normalBorderColor  = "#000" -- "#dddddd"
    , focusedBorderColor = "#999"    -- "#ff0000" don't use hex, not <24 bit safe
    , manageHook = manageHook bluetileConfig <+> myManageHook <+> namedScratchpadManageHook scratchpads
    , focusFollowsMouse  = True
    , layoutHook = bluetileLayoutHook
    , startupHook = ewmhDesktopsStartup <+> myStartup
    , modMask = modm
    , mouseBindings = newMouse
    , workspaces = myTopics }
  `additionalKeys` myAdditionalKeys
  -- }}}

-- {{{ MAIN
main = xmonad =<< xmobar myConfig
-- }}}

-- vim: foldmethod=marker
