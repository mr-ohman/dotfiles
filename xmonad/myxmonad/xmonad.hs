
import Control.Monad

import qualified Data.Map        as M

import Graphics.X11.ExtraTypes.XF86
import Graphics.X11.ExtraTypes.XorgDefault

import System.Exit
import System.IO
import System.Posix.Unistd

import XMonad hiding ( (|||) )
import qualified XMonad.StackSet as W

import XMonad.Actions.CopyWindow
import XMonad.Actions.NoBorders
import XMonad.Actions.DynamicProjects
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.SpawnOn
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Actions.CycleWS
import XMonad.Actions.WithAll
import XMonad.Actions.Submap

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.FixedColumn
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Reflect
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.LayoutCombinators

import XMonad.Prompt
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Pass
import XMonad.Prompt.ConfirmPrompt

import XMonad.Util.EZConfig
import XMonad.Util.Replace
import XMonad.Util.Run


------------------------------------------------------------------------
-- Application settings
------------------------------------------------------------------------

myTerminal      = "/usr/bin/urxvt"
myNixTerminal   = "urxvt"
myLauncher      = "rofi -matching fuzzy -show run"
myEditor        = "emacs"
myEditTerminal  = "/usr/bin/urxvt -bg '#313131' -fg '#dcdccc' +tr"
myWebBrowser    = "firefox"
myFileBrowser   = "thunar"
myPDFReader     = "okular"
myReadTerminal  = "/usr/bin/urxvt -bg '#eeeeee' -fg '#020202' +tr"
myNotepad       = "gedit"

------------------------------------------------------------------------
-- Theme settings
------------------------------------------------------------------------

softblack = "#1b1d1e"
darkgray  = "#313131"
gray      = "#7b7b7b"
white     = "#ffffff"
softwhite = "#dddddd"
red       = "#ff0000"
orange    = "#ebac54"

myFont    = "xft:Mono:pixelsize=15"

barHeight = 24
barWidth  = 860

myBorderWidth   = 1
myNormalBorderColor  = softwhite
myFocusedBorderColor = orange
myFocusFollowsMouse = False

myLogHook h = dynamicLogWithPP $ def
  { ppCurrent         = dzenColor orange softblack . pad
  , ppVisible         = dzenColor white softblack . pad
  , ppHidden          = dzenColor white softblack . pad
  , ppHiddenNoWindows = dzenColor gray softblack . pad
  , ppUrgent          = dzenColor "#37C0F4" softblack . pad
  , ppWsSep           = " "
  , ppSep             = "  |  "
  , ppLayout          = dzenColor orange softblack
  , ppTitle           = (" " ++) . dzenColor white softblack . dzenEscape
  , ppOutput          = hPutStrLn h
  , ppSort            = DO.getSortByOrder
  }

myTabTheme = def
  { fontName            = myFont
  , activeColor         = darkgray
  , inactiveColor       = softblack
  , activeBorderColor   = orange
  , inactiveBorderColor = softwhite
  , activeTextColor     = orange
  , inactiveTextColor   = white
  }

myPromptTheme = def
  { font              = myFont
  , bgColor           = softblack
  , fgColor           = white
  , fgHLight          = orange
  , bgHLight          = darkgray
  , borderColor       = orange
  , promptBorderWidth = myBorderWidth
  , height            = barHeight
  , position          = Bottom
  }

hotPromptTheme = myPromptTheme
  { bgColor          = orange
  , fgColor          = softblack
  , borderColor      = softblack
  }

myXmonadBar = "dzen2 -x '0' -y '0' -ta 'l'" ++ myCommonBar

myStatusBar =
  concat [ "conky -c ~/.xmonad/conky_dzen | dzen2"
         , " -x '", show barWidth, "'"
         , " -y '0' -ta 'r'"
         ] ++ myCommonBar

myCommonBar =
  concat [ " -w '", show barWidth, "'"
         , " -h '", show barHeight, "'"
         , " -fg '", white, "'"
         , " -bg '", softblack, "'"
         , " -e 'onstart=lower' -dock"
         ]

------------------------------------------------------------------------
-- Projects and workspaces
------------------------------------------------------------------------

startingWorkspaces = ["."]

projects =
  [ project "WEB"  False [myWebBrowser]
  , project "MAIL" False ["thunderbird"]
  , project "EDT"  True  [myEditor]
  , project "GIT"  True  ["git gui", "gitk"]
  , project "TEX"  True  [myEditor, myPDFReader]
  , project "LYX"  True  ["lyx", myPDFReader]
  , project "DOC"  False [myPDFReader, myReadTerminal, myFileBrowser]
  , project "VID"  False ["vlc"]
  , project "COM"  False ["telegram-desktop", "discord"]
  , project "STM"  False ["steam"]
  , project "SYS"  False [myTerminal, myTerminal]
  , project "REM"  False [myTerminal, myTerminal]
  ]

changeProjectDirIfHome sh = do
  p <- currentProject
  if projectDirectory p == "~/"
  then changeProjectDirPrompt myPromptTheme
  else sh

project name cpd ps =
  Project { projectName = name
          , projectDirectory = "~/"
          , projectStartHook = startHook
          }
  where startHook = if null ps then nops else withps
        nops = if cpd then Just (changeProjectDirIfHome (return ())) else Nothing
        withps = (if cpd then Just . changeProjectDirIfHome else Just) (programs ps)
        programs [p] = spawnOn name p
        programs (p:ps) = spawnOn name p >> programs ps

------------------------------------------------------------------------
-- Key bindings
------------------------------------------------------------------------

myKeys conf = mkKeymap conf $ mainKeymap conf

-- Below originally from:
-- https://blog.pclewis.com/2016/03/19/xmonad-spacemacs-style.html

keyMapDoc :: String -> X Handle
keyMapDoc name = do
  -- focused screen location/size
  r <- withWindowSet $ return . screenRect . W.screenDetail . W.current

  handle <- spawnPipe $ unwords [ "~/.xmonad/showHintForKeymap.sh"
                                , name
                                , show (rect_x r)
                                , show (rect_y r)
                                , show (rect_width r)
                                , show (rect_height r)
                                , show orange    -- key color
                                , show white     -- cmd color
                                , "18"           -- line height
                                ]

  return handle

toSubmap :: XConfig l -> String -> [(String, X ())] -> X ()
toSubmap c name m = do
  pipe <- keyMapDoc name
  submap $ mkKeymap c m
  io $ hClose pipe

myModMask = mod4Mask

wsKeys = map (:[]) ['1'..'9'] ++ ["0","-","="]

mainKeymap c =
  -- Mode keys
  [ ("M-/",  toSubmap c "mainKeymap" (mainKeymap c)) -- Main Menu
  , ("M-q",  toSubmap c "quitKeymap" quitKeymap) -- Quit
  , ("M-s",  toSubmap c "systemKeymap" systemKeymap) -- System
  , ("M-x",  toSubmap c "executeKeymap" executeKeymap) -- Execute
  , ("M-w",  toSubmap c "windowKeymap" windowKeymap) -- Window
  , ("M-p",  toSubmap c "projectKeymap" projectKeymap) -- Project
  , ("M-l",  toSubmap c "layoutKeymap" layoutKeymap) -- Layout
  , ("M-m",  toSubmap c "musicKeymap" musicKeymap) -- Music
  , ("M-d",  toSubmap c "displayKeymap" displayKeymap) -- Display
  ] ++ hiddenKeys
  where hiddenKeys =
          [ ("<XF86AudioMute>",        spawn "amixer set Master toggle") -- Mute
          , ("M-<Up>",                 spawn "amixer set Master 2%+") -- Volume up
          , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+") -- Volume up
          , ("M-<Down>",               spawn "amixer set Master 2%-") -- Volume down
          , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-") -- Volume down
          , ("<XF86AudioPlay>", spawn "mpc toggle") -- Play/Pause
          , ("<XF86AudioNext>", spawn "mpc next") -- Play next
          , ("<XF86AudioPrev>", spawn "mpc prev") -- Play previous
          , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2") -- Light down
          , ("<XF86MonBrightnessUp>",   spawn "xbacklight -inc 2") -- Light up
          , ("<Print>", spawn "scrot") -- Print screen
          , ("C-<Print>", spawn "sleep 0.2; scrot -s") -- Print window
          -- misc system
          , ("M-<F7>", spawn "sleep 0.2; xset dpms force off")
          , ("M-<F8>", spawn "trackpad-toggle.sh")
          -- toggle statusbar
          , ("M-b", sendMessage ToggleStruts)

          -- movement
          , ("M-k", windows W.focusUp)
          , ("M-j", windows W.focusDown)
          , ("M-S-k", windows W.swapUp)
          , ("M-S-j", windows W.swapDown)
          , ("M-C-k", DO.swapWith Next NonEmptyWS)
          , ("M-C-j", DO.swapWith Prev NonEmptyWS)

          -- workspaces
          , ("M-`", removeEmptyWorkspaceAfter $ switchProjectPrompt myPromptTheme)
          , ("M-S-`", shiftToProjectPrompt myPromptTheme)
          ] ++
          [ ("M-" ++ m ++ k, g $ DO.withNthWorkspace f i)
          | (i, k) <- zip [0..] wsKeys
          , (f, g, m) <- [ (W.greedyView, removeEmptyWorkspaceAfter, "")
                         , (W.shift, id, "S-")]
          ] ++
          [ ("M-+", removeEmptyWorkspaceAfter $ DO.withNthWorkspace W.greedyView 10)
          , ("M-S-+", DO.withNthWorkspace W.shift 10)
          ]

-- Quit (M-q)
quitKeymap =
  [ ("q",   quitXmonad) -- Quit
  , ("r",   restartXmonad) -- Restart
  , ("S-r", rebuildXmonad) -- Rebuild
  , ("o",   restart "~/.xmonad/obtoxmd" True) -- Openbox
  ]
  where quitXmonad = confirmPrompt hotPromptTheme "quit xmonad" $ io (exitWith ExitSuccess)
        restartXmonad = spawn "killall dzen2; xmonad --restart"
        rebuildXmonad = spawn "killall dzen2; ~/.xmonad/build.sh; xmonad --restart"

-- System (M-s)
systemKeymap =
  [ ("z", spawn "xscreensaver-command -lock") -- Lock screen
  , ("s", spawn "systemctl suspend -i") -- Suspend
  ]

-- Execute (M-x)
executeKeymap =
  [ ("x", spawn myLauncher) -- Launcher
  , ("t", spawn myTerminal) -- Terminal
  , ("S-t", spawn myNixTerminal) -- Nix Terminal
  , ("w", spawn myWebBrowser) -- Web browser
  , ("e", spawn myEditor) -- Editor
  , ("r", spawn myPDFReader) -- PDF reader
  , ("f", spawn myFileBrowser) -- File browser
  , ("p s", passPrompt myPromptTheme) -- Select password
  , ("p g", passGeneratePrompt myPromptTheme) -- Generate password
  ]

-- Window (M-w)
windowKeymap =
  [ ("x", kill1) -- Close
  , ("r", refresh) -- Reset size
  , ("k", windows W.swapUp)
  , ("j", windows W.swapDown)
  , ("t", withFocused $ windows . W.sink)
  , ("m", windows W.swapMaster)
  , ("b", withFocused toggleBorder) -- Toggle border
  , ("d", killAllOtherCopies)
  , ("c a", windows copyToAll)
  ] ++
  [ ("c " ++ k, DO.withNthWorkspace copy i)
  | (i, k) <- zip [0..] wsKeys
  ]


-- Project (M-p)
projectKeymap =
  [ ("x", closeProject) -- Close
  , ("r", renameProjectPrompt myPromptTheme) -- Rename
  , ("d", changeProjectDirPrompt myPromptTheme) -- Change directory
  ]
  where closeProject = confirmPrompt hotPromptTheme "close project" $ killAll >> removeWorkspace

-- Layout (M-l)
layoutKeymap =
  [ ("k", sendMessage NextLayout)
  , ("h", sendMessage Shrink)
  , ("l", sendMessage Expand)
  , ("w", sendMessage $ IncMasterN 1)
  , ("d", sendMessage $ IncMasterN (-1))
  ]

-- Music (M-m)
musicKeymap =
  [ ("p", spawn "mpc toggle") -- Play/Pause
  , ("j", spawn "mpc next") -- Next
  , ("k", spawn "mpc prev") -- Previous
  ]

-- Display (M-d)
displayKeymap =
  [ ("o", spawn "mons -o")
  , ("s", spawn "mons -s")
  , ("d", spawn "mons -d")
  , ("m", spawn "mons -m")
  , ("l", spawn "mons -e left")
  , ("r", spawn "mons -e right")
  , ("t", spawn "mons -e top")
  , ("b", spawn "mons -e bottom")
  ]

extraKeys =
  -- Fix for dead acute not being in EZConfig
  [ ( (myModMask, xK_dead_acute)
    , removeEmptyWorkspaceAfter $ DO.withNthWorkspace W.greedyView 11)
  , ( (myModMask .|. shiftMask, xK_dead_acute), DO.withNthWorkspace W.shift 11)
  ]

    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    -- let monitorKeys = ["w","e","r"]
    -- in  [("M-" ++ m ++ key, screenWorkspace sc >>= flip whenJust (windows . f))
    --       | (key, sc) <- zip monitorKeys [0..]
    --       , (f, m) <- [(W.view, ""), (W.shift, "S-")]]

    --, ((modm,               xK_Left),  withFocused $ snapMove L Nothing)
    --, ((modm,               xK_Right), withFocused $ snapMove R Nothing)
    --, ((modm,               xK_Up),    withFocused $ snapMove U Nothing)
    --, ((modm,               xK_Down),  withFocused $ snapMove D Nothing)
    --, ((modm .|. shiftMask, xK_Left),  withFocused $ snapShrink R Nothing)
    --, ((modm .|. shiftMask, xK_Right), withFocused $ snapGrow R Nothing)
    --, ((modm .|. shiftMask, xK_Up),    withFocused $ snapShrink D Nothing)
    --, ((modm .|. shiftMask, xK_Down),  withFocused $ snapGrow D Nothing)

------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
------------------------------------------------------------------------

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
------------------------------------------------------------------------

myLayout = smartBorders $
           avoidStruts $
           onWorkspace "WEB" tabbedFirst $
           onWorkspace "EDT" tabbedFirst $
           onWorkspace "DOC" readLayout $
           onWorkspace "VID" fullFirst $
           onWorkspace "STM" fullFirst $
           defaultOrder
  where
    -- Regular layouts
    tiledLayout  = named "Tiled"   $ tiled
    mirrorLayout = named "Mirror"  $ Mirror tiled
    tabbedLayout = named "Tabbed"  $ tabbed shrinkText myTabTheme
    fullLayout   = noBorders Full

    -- Special layouts
    -- editorLayout = named "Editor"  $ reflectHoriz $ FixedColumn 1 20 164 10
    readLayout   = named "Reading" $ Tall 1 (3/100) (5/6)

    named x      = renamed [Replace x]
    tiled        = Tall 1 (3/100) (1/2)

    -- Orders
    defaultOrder = tiledFirst
    tiledFirst   =
      tiledLayout  ||| mirrorLayout ||| tabbedLayout ||| fullLayout
    tabbedFirst  =
      tabbedLayout ||| fullLayout   ||| tiledLayout  ||| mirrorLayout
    fullFirst    =
      fullLayout   ||| tiledLayout  ||| mirrorLayout ||| tabbedLayout

------------------------------------------------------------------------
-- Window rules:
------------------------------------------------------------------------

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
------------------------------------------------------------------------

myEventHook = docksEventHook

------------------------------------------------------------------------
-- Startup hook
------------------------------------------------------------------------

myStartupHook conf = do setWMName "LG3D"
                        return () >> checkKeymap conf (mainKeymap conf)

------------------------------------------------------------------------
-- Main program
------------------------------------------------------------------------

main = do replace
          dzenLeftBar  <- spawnPipe myXmonadBar
          spawn myStatusBar
          xmonad $
            dynamicProjects projects $
            withUrgencyHook NoUrgencyHook $
            ewmh $
            defaults dzenLeftBar

defaults logBar = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = startingWorkspaces,
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
        startupHook        = myStartupHook (defaults logBar)
    }
    `additionalKeys` extraKeys

------------------------------------------------------------------------
-- Extra utils
------------------------------------------------------------------------

dzenClickable :: String -> String -> String
dzenClickable action label = "^ca(1," ++ action ++ ")" ++ label ++ "^ca()"
