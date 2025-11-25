import Control.Monad (when)
import Data.Ratio ((%))
import qualified Data.Map as M
import qualified XMonad.StackSet as W

import XMonad
import XMonad.Config.Bluetile

-- actions
import XMonad.Actions.OnScreen
import XMonad.Actions.CycleRecentWS (cycleRecentWS)
import XMonad.Actions.DwmPromote (dwmpromote)
import XMonad.Actions.PhysicalScreens
  ( PhysicalScreen
  , ScreenComparator
  , getScreen
  , horizontalScreenOrderer
  , sendToScreen
  , viewScreen
  )
import XMonad.Actions.Warp (warpToScreen)
import XMonad.Actions.WorkspaceNames(workspaceNamesEwmh)

-- ewmh / hooks 
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.Rescreen (addAfterRescreenHook)
import XMonad.Hooks.UrgencyHook

-- layouts
import XMonad.Layout.BoringWindows
import XMonad.Layout.DecorationMadness
import XMonad.Layout.Grid
import XMonad.Layout.LayoutCombinators ((|||))
import XMonad.Layout.Minimize
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Named
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.BorderResize
import XMonad.Layout.PositionStoreFloat
import XMonad.Layout.Reflect
import XMonad.Layout.TwoPane

-- prompts
import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Workspace

-- utils
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Util.Run (spawnPipe)

--------------------------------------------------------------------------------
-- Workspaces

leftPinned, rightPinned, flexTopics :: [(WorkspaceId, KeySym)]
leftPinned =
  zip ["1","2","3","4","5","6","7"] [xK_1..xK_7]

rightPinned =
  [ ("8", xK_8), ("9", xK_9), ("0", xK_0)
  , ("=", xK_backslash)
  ]

flexTopics =
  [ ("u", xK_u), ("i", xK_i), ("o", xK_o), ("p", xK_p)
  , ("z", xK_z), ("x", xK_x), ("c", xK_c), ("v", xK_v)
  ]

myWorkspaces :: [WorkspaceId]
myWorkspaces = map fst leftPinned ++ map fst rightPinned ++ map fst flexTopics

--------------------------------------------------------------------------------
-- Screens

modm :: KeyMask
modm = mod1Mask  -- Alt

screenOrder :: ScreenComparator
screenOrder = horizontalScreenOrderer

leftScreen, rightScreen :: PhysicalScreen
leftScreen  = 0
rightScreen = 1

defaultRightWorkspace :: WorkspaceId
defaultRightWorkspace = "="

isMultiScreen :: X Bool
isMultiScreen = (>= 2) . length . W.screens <$> gets windowset

-- Change *a specific physical screen's* workspace, without moving focus.
viewOnPhysical :: PhysicalScreen -> WorkspaceId -> X ()
viewOnPhysical ps ws = do
  msid <- getScreen screenOrder ps
  case msid of
    Nothing  -> windows (W.greedyView ws)      -- fallback
    Just sid -> windows (viewOnScreen sid ws)  -- update that screen only

-- For pinned workspaces: update a specific screen, but keep keyboard focus where it is.
gotoPinned :: PhysicalScreen -> WorkspaceId -> X ()
gotoPinned ps ws = do
  multi <- isMultiScreen
  if multi then viewOnPhysical ps ws else windows (W.greedyView ws)

-- Warp only when explicitly moving between monitors.
focusAndWarp :: PhysicalScreen -> X ()
focusAndWarp ps = do
  multi <- isMultiScreen
  when multi $ do
    viewScreen screenOrder ps
    msid <- getScreen screenOrder ps
    case msid of
      Nothing  -> pure ()
      Just sid -> warpToScreen sid (1%2) (1%2)

ensureRightMonitorDefault :: X ()
ensureRightMonitorDefault = do
  multi <- isMultiScreen
  when multi $ viewOnPhysical rightScreen defaultRightWorkspace

--------------------------------------------------------------------------------
-- Layout

myLayoutHook = renamed [CutWordsLeft 1] $
  mkToggle1 NBFULL $
  mkToggle1 REFLECTX $
  mkToggle1 REFLECTY $
  mkToggle1 NOBORDERS $
  avoidStrutsOn [U] $
  boringWindows $
  minimize $
  lessBorders Screen $
    named "Fullscreen" Full |||
    named "Tiled" tiled |||
    named "TwoPane" (TwoPane 0.03 0.5) |||
    named "Grid" Grid |||
    named "Floating" floating
 where
  tiled = mouseResizableTile { draggerType = BordersDragger }
  floating = floatingDeco $ borderResize $ positionStoreFloat
  floatingDeco _l = floatSimpleSimple

--------------------------------------------------------------------------------
-- Manage hook

myManageHook :: ManageHook
myManageHook = composeAll
  [ resource =? "desktop_window" --> doIgnore
  , resource =? "Desktop"        --> doFullFloat
  , className =? "Workrave"      --> doIgnore
  , className =? "onboard"       --> doIgnore
  , className =? "xfce4-panel"   --> doIgnore
  ]


--------------------------------------------------------------------------------
-- Mousebindings
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
--------------------------------------------------------------------------------
-- Keybindings

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((controlMask, xK_semicolon), spawn "dmenu_run -b -nb black -nf white")
  , ((controlMask, xK_space),     spawn "/usr/bin/rofi -combi-modi drun,run -show combi -modi combi")

  , ((modm, xK_space), dwmpromote)

  , ((modm, xK_a), sendMessage $ JumpToLayout "Floating")
  , ((modm, xK_s), sendMessage $ JumpToLayout "Tiled")
  , ((modm, xK_d), sendMessage $ JumpToLayout "TwoPane")
  , ((modm, xK_f), sendMessage $ JumpToLayout "Fullscreen")
  , ((modm, xK_g), sendMessage $ JumpToLayout "Grid")

  -- layout modifiers
  , ((modm, xK_b), sendMessage $ ToggleStruts)
  , ((modm .|. controlMask, xK_space), sendMessage $ Toggle NBFULL)
  , ((modm .|. controlMask, xK_x),     sendMessage $ Toggle REFLECTX)
  , ((modm .|. controlMask, xK_y),     sendMessage $ Toggle REFLECTY)
  , ((modm .|. controlMask, xK_b),     sendMessage $ Toggle NOBORDERS)

  , ((modm, xK_Delete), kill)

  -- rebind shrink/expand because they used to be h/l
  , ((modm .|. shiftMask, xK_comma), sendMessage Shrink)
  , ((modm .|. shiftMask, xK_period), sendMessage Expand)

  -- warp-only monitor focus
  , ((modm, xK_h), focusAndWarp leftScreen)
  , ((modm, xK_l), focusAndWarp rightScreen)

  -- move focused window to a monitor
  , ((modm .|. shiftMask, xK_h), sendToScreen screenOrder leftScreen)
  , ((modm .|. shiftMask, xK_l), sendToScreen screenOrder rightScreen)

  -- brightness
  , ((0, xK_F5), spawn "xbacklight -dec 2")
  , ((0, xK_F6), spawn "xbacklight -inc 2")
  ]
  ++
  -- left pinned: mod/ctrl = view on left screen; +shift = move window
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
  -- right pinned
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
  -- flex: mod = view on current; mod+shift = move window
  [ ((modm, k), windows (W.greedyView ws)) | (ws, k) <- flexTopics ]
  ++
  [ ((modm .|. shiftMask, k), windows (W.shift ws)) | (ws, k) <- flexTopics ]

--------------------------------------------------------------------------------
-- Config

myStartup :: X ()
myStartup = spawn "bash ~/.xinitrc &"

main :: IO ()
main = xmonad finalConfig

finalConfig =
    addRescreen
  $ addUrgency
  $ addEwmh
  $ workspaceNamesEwmh
  $ baseConfig
  where
    addRescreen = addAfterRescreenHook ensureRightMonitorDefault
    addUrgency  = withUrgencyHook NoUrgencyHook
    addEwmh     = ewmh

baseConfig =
  (bluetileConfig
     { borderWidth        = 2
     , normalBorderColor  = "#000"
     , focusedBorderColor = "#999"
     , manageHook         = manageHook bluetileConfig <+> myManageHook
     , focusFollowsMouse  = True
     , layoutHook         = noBorders myLayoutHook
     , startupHook        = myStartup <+> ensureRightMonitorDefault
     , modMask            = modm
     , mouseBindings      = newMouse
     , workspaces         = myWorkspaces
     })
     `additionalKeys` myKeys
