
import XMonad

import XMonad.Actions.NoBorders
import XMonad.Actions.CopyWindow

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Layout.Combo
import XMonad.Layout.FixedColumn
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Tabbed
import XMonad.Layout.TwoPane
import XMonad.Layout.ToggleLayouts

import XMonad.Util.EZConfig
import XMonad.Util.Replace
import XMonad.Util.Run

import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Pass

import XMonad.Hooks.UrgencyHook

import qualified XMonad.StackSet as W

import qualified Data.Map        as M

import Control.Monad

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.ExtraTypes.XorgDefault

import System.Exit
import System.IO
import System.Posix.Unistd

------------------------------------------------------------------------
-- Standard settings
--
myTerminal      = "urxvt"
myModMask       = mod4Mask

myFocusFollowsMouse = False

myBorderWidth   = 1
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#ff0000"

myWorkspaces = ["1","2","3","4","5","6","7","8","9","0","-","="]

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeymap conf =

    -- launch a terminal
    [ ("M-S-<Return>", spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ("M-p", spawn "dmenu_run")

    -- close focused window
    , ("M-S-c", kill)

    -- Rotate through the available layout algorithms
    , ("M-<Space>", sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ("M-C-<Space>", sendMessage ToggleLayout)

    --  Reset the layouts on the current workspace to default
    , ("M-S-<Space>", setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ("M-n", refresh)

    -- Move focus to the next window
    , ("M-<Tab>", windows W.focusDown)
    , ("M-k", windows W.focusDown)

    -- Move focus to the previous window
    , ("M-j", windows W.focusUp)

    -- Move focus to the master window
    , ("M-m", windows W.focusMaster)

    -- Swap the focused window and the master window
    , ("M-<Return>", windows W.swapMaster)

    -- Swap the focused window with the next window
    , ("M-S-k", windows W.swapDown)

    -- Swap the focused window with the previous window
    , ("M-S-j", windows W.swapUp)

    -- Shrink the master area
    , ("M-h", sendMessage Shrink)

    -- Expand the master area
    , ("M-l", sendMessage Expand)

    -- Push window back into tiling
    , ("M-t", withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ("M-,", sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ("M-.", sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    , ("M-b", sendMessage ToggleStruts)

    -- Quit xmonad
    , ("M-S-q", io (exitWith ExitSuccess))

    -- Restart xmonad
    , ("M-q", spawn "xmonad --recompile; xmonad --restart")

    -- Toggle mute
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")

    -- Increase volume
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")
    , ("M-<Up>", spawn "amixer set Master 2%+")

    -- Decrease volume
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")
    , ("M-<Down>", spawn "amixer set Master 2%-")

    , ("<XF86AudioPlay>", spawn "mpc toggle")
    , ("<XF86AudioPrev>", spawn "mpc prev")
    , ("<XF86AudioNext>", spawn "mpc next")

    -- Lock screen
    , ("M-S-z", spawn "xscreensaver-command -lock")

    -- Suspend system
    , ("M-S-s", spawn "systemctl suspend")

    -- Switch to Openbox
    , ("M-S-o", restart "/home/joakim/.xmonad/obtoxmd" True)

    -- Print screen
    , ("<Print>", spawn "scrot")

    -- Print a selected window
    , ("C-<Print>", spawn "sleep 0.2; scrot -s")

    -- Launch application
    , ("M-x e", spawn "emacsclient -c -a \"\"")
    , ("M-x c", spawn "chromium")
    , ("M-x f", spawn "firefox")

    -- Take a note
    , ("M-a", appendFilePrompt def "/home/joakim/NOTES")

    -- Password bindings
    , ("M-S-p", passPrompt def)
    , ("M-S-C-p", passGeneratePrompt def)

    ---- Laptop function keys
    -- Blank screen
    , ("M-<F7>", spawn "sleep 0.2; xset dpms force off")

    -- Toggle touchpad
    , ("M-<F8>", spawn "/home/joakim/.xmonad/trackpad-toggle.sh")

    -- Decrease backlight
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2")

    -- Increase backlight
    , ("<XF86MonBrightnessUp>",   spawn "xbacklight -inc 2")

    -- Switch border on current window
    , ("M-g", withFocused toggleBorder)

    -- Check keymap
    , ("M-x x k", return () >> checkKeymap conf (myKeymap conf))

    -- Display window on multiple
    , ("M-v", windows copyToAll) -- @@ Make focused window always visible
    , ("M-S-v",  killAllOtherCopies) -- @@ Toggle window state back


    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [("M-" ++ m ++ k, windows $ f i)
          | (i, k) <- zip (XMonad.workspaces conf) myWorkspaces
          , (f, m) <- [(W.greedyView, ""), (W.shift, "S-")]]
    ++ [ ("M-+", windows $ W.greedyView (myWorkspaces !! 10))
       , ("M-S-+", windows $ W.shift (myWorkspaces !! 10))
       ]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    let monitorKeys = ["w","e","r"]
    in  [("M-" ++ m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
          | (key, sc) <- zip monitorKeys [0..]
          , (f, m) <- [(W.view, ""), (W.shift, "S-")]]

    --, ((modm,               xK_Left),  withFocused $ snapMove L Nothing)
    --, ((modm,               xK_Right), withFocused $ snapMove R Nothing)
    --, ((modm,               xK_Up),    withFocused $ snapMove U Nothing)
    --, ((modm,               xK_Down),  withFocused $ snapMove D Nothing)
    --, ((modm .|. shiftMask, xK_Left),  withFocused $ snapShrink R Nothing)
    --, ((modm .|. shiftMask, xK_Right), withFocused $ snapGrow R Nothing)
    --, ((modm .|. shiftMask, xK_Up),    withFocused $ snapShrink D Nothing)
    --, ((modm .|. shiftMask, xK_Down),  withFocused $ snapGrow D Nothing)

myKeys conf = mkKeymap conf $ myKeymap conf

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- * NOTE: XMonad.Hooks.EwmhDesktops users must remove the obsolete
-- ewmhDesktopsLayout modifier from layoutHook. It no longer exists.
-- Instead use the 'ewmh' function from that module to modify your
-- defaultConfig as a whole. (See also logHook, handleEventHook, and
-- startupHook ewmh notes.)
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = smartBorders $ defaultLayout
  where
    -- default Layout
    defaultLayout =
      avoidStruts (tiled ||| Mirror tiled ||| simpleTabbed ||| Grid |||
                   editorLayout ||| browserLayout ||| noBorders Full)

    -- browser Layout
    browserLayout =
      toggleLayouts (noBorders Full)
                    (combineTwo (TwoPane delta 0.8) browserTiled Full)

    -- editor Layout
    editorLayout = FixedColumn 1 20 84 10

    -- default tiling algorithm partitions the screen into two panes
    tiled        = Tall 1 delta ratio
    browserTiled = let ratioTabs = 194/200
                   in  reflectVert (Mirror (Tall 1 (1/200) ratioTabs))

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = manageDocks <+> (composeAll . concat $
    [ [transience']
    , [isDialog --> doFloat]
    , [className =? c --> doFloat | c <- myCFloats]
    , [title =? t --> doFloat | t <- myTFloats]
    , [resource =? r --> doFloat | r <- myRFloats]
    , [className =? c --> doCenterFloat | c <- myCCFloat]
    , [className =? c --> myDoFullFloat | c <- myCFFloat]
    , [resource =? i --> doIgnore | i <- myIgnores]
    ])
    where
      myCFloats = ["MPlayer", "GIMP", "XVkbd"]
      myTFloats = []
      myRFloats = []
      myCCFloat = []
      myCFFloat = []
      myIgnores = ["desktop_window", "kdesktop"]

-- a trick for fullscreen but still allow focusing of other WSs
myDoFullFloat :: ManageHook
myDoFullFloat = doF W.focusDown <+> doFullFloat

------------------------------------------------------------------------
-- Event handling

-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH event handling to your custom event hooks by
-- combining them with ewmhDesktopsEventHook.
--
myEventHook = docksEventHook

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add EWMH logHook actions to your custom log hook by
-- combining it with ewmhDesktopsLogHook.
--
myLogHook h = dynamicLogWithPP $ def
    {
        ppCurrent           =   dzenColor "#ebac54" "#1B1D1E" . pad
      , ppVisible           =   dzenColor "white" "#1B1D1E" . pad
      , ppHidden            =   dzenColor "white" "#1B1D1E" . pad
      , ppHiddenNoWindows   =   dzenColor "#7b7b7b" "#1B1D1E" . pad
      , ppUrgent            =   dzenColor "#37C0F4" "#1B1D1E" . pad
      , ppWsSep             =   " "
      , ppSep               =   "  |  "
      , ppLayout            =   dzenColor "#ebac54" "#1B1D1E" .
                                (\x -> case x of
                                    "combining ReflectY Mirror Tall and Full with TwoPane" -> "Browser"
                                    _ -> x
                                )
      , ppTitle             =   (" " ++) . dzenColor "white" "#1B1D1E" . dzenEscape
      , ppOutput            =   hPutStrLn h
    }

{-
. (\x -> case x of
           "ResizableTall"        -> "^i(" ++ myBitmapsDir ++ "/tall.xbm)"
           "Mirror ResizableTall" -> "^i(" ++ myBitmapsDir ++ "/mtall.xbm)"
           "Full"                 -> "^i(" ++ myBitmapsDir ++ "/full.xbm)"
           "Simple Float"         -> "~"
           _                      -> x
)-}

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
--
-- * NOTE: EwmhDesktops users should use the 'ewmh' function from
-- XMonad.Hooks.EwmhDesktops to modify their defaultConfig as a whole.
-- It will add initialization of EWMH support to your custom startup
-- hook by combining it with ewmhDesktopsStartup.
--

myStartupHook = setWMName "LG3D"

------------------------------------------------------------------------

myXmonadBar =
  let w = "860"
  in  unwords [ "dzen2 -x '0' -y '0' -h '24' -w '" ++ w ++ "' -ta 'l'"
              , "-fg '#FFFFFF' -bg '#1B1D1E' -e 'onstart=lower'"
              ]

myStatusBar =
  let x = "860"
      w = "860"
  in  unwords [ "conky -c /home/joakim/.xmonad/conky_dzen | dzen2"
              , "-x '" ++ x ++ "' -y '0' -w '" ++ w ++ "' -h '24' -ta 'r'"
              , "-bg '#1B1D1E' -fg '#FFFFFF' -e 'onstart=lower'"
              ]

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do replace
          dzenLeftBar  <- spawnPipe myXmonadBar
          spawn myStatusBar
          xmonad $ withUrgencyHook NoUrgencyHook $ defaults dzenLeftBar

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults logBar = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook logBar,
        startupHook        = myStartupHook
    }
    `additionalKeys`
    -- Fix for dead acute not being in EZConfig
    [ ((myModMask, xK_dead_acute) , windows $ W.greedyView (myWorkspaces !! 11))
    , ((myModMask .|. shiftMask, xK_dead_acute) , windows $ W.shift (myWorkspaces !! 11))
    ]

------------------------------------------------------------------------
-- Extra utils

dzenClickable :: String -> String -> String
dzenClickable action label = "^ca(1," ++ action ++ ")" ++ label ++ "^ca()"
