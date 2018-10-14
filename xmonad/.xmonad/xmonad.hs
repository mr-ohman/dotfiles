
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

myTerminal      = "urxvt"
myLauncher      = "rofi -matching fuzzy -show run"
myEditor        = "emacs"
myEditTerminal  = "urxvt -bg '#313131' -fg '#dcdccc' +tr"
myWebBrowser    = "firefox"
myFileBrowser   = "thunar"
myPDFReader     = "okular"
myReadTerminal  = "urxvt -bg '#eeeeee' -fg '#020202' +tr"
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
  -- Web browsing
  [ Project { projectName = "WEB"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawnOn "WEB" myWebBrowser
            }
  -- Editor
  , Project { projectName = "EDT"
            , projectDirectory = "~/"
            , projectStartHook = Just $ changeProjectDirIfHome $ do
                                        spawnOn "EDT" myEditor
                                        spawnOn "EDT" myEditTerminal
                                        spawnOn "EDT" myEditTerminal
                                        spawnOn "EDT" myEditTerminal
            }
   -- GIT version control
  , Project { projectName = "GIT"
            , projectDirectory = "~/"
            , projectStartHook = Just $ changeProjectDirIfHome $ do
                                        spawnOn "GIT" "git gui"
                                        spawnOn "GIT" "gitk"
            }
  -- Document reading
  , Project { projectName = "DOC"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawnOn "DOC" myPDFReader
                                           spawnOn "DOC" myNotepad
                                           spawnOn "DOC" myReadTerminal
                                           spawnOn "DOC" myFileBrowser
            }
  -- Remote access
  , Project { projectName = "REM"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawnOn "REM" myTerminal
                                           spawnOn "REM" myTerminal
            }
  -- Video watching
  , Project { projectName = "VID"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawnOn "VID" "vlc"
            }
  -- Communication
  , Project { projectName = "COM"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawnOn "COM" "telegram-desktop"
            }
  -- Steam
  , Project { projectName = "STM"
            , projectDirectory = "~/"
            , projectStartHook = Just $ spawnOn "STM" "steam"
            }
  -- System management
  , Project { projectName = "SYS"
            , projectDirectory = "~/"
            , projectStartHook = Just $ do spawnOn "SYS" myTerminal
                                           spawnOn "SYS" myTerminal
            }
  ]

changeProjectDirIfHome sh = do
  p <- currentProject
  if projectDirectory p == "~/"
  then changeProjectDirPrompt myPromptTheme
  else sh

------------------------------------------------------------------------
-- Key bindings
------------------------------------------------------------------------

myModMask = mod4Mask

wsKeys = map (:[]) ['1'..'9'] ++ ["0","-","="]

-- TODO: Add showing of the keymap a la spacemacs and make M
-- xmonads leader key
myKeys conf = mkKeymap conf $ myKeymap conf
myKeymap conf =
  -- M-q, [q]uit or restart xmonad
  [ ("M-q q", confirmPrompt hotPromptTheme "quit xmonad" $ quitXmonad)
  , ("M-q r", restartXmonad)
  , ("M-q S-r", rebuildXmonad)
  , ("M-q o", restart "~/.xmonad/obtoxmd" True)

  -- M-s, [s]ystem
  , ("M-s z", spawn "xscreensaver-command -lock")
  , ("M-s s", spawn "systemctl suspend -i")

  -- M-x, e[x]ecute program and misc utils
  , ("M-x x", spawn myLauncher)
  , ("M-x t", spawn myTerminal)
  , ("M-x w", spawn myWebBrowser)
  , ("M-x e", spawn myEditor)
  , ("M-x r", spawn myPDFReader)
  , ("M-x f", spawn myFileBrowser)
  , ("M-x p s", passPrompt myPromptTheme)
  , ("M-x p g", passGeneratePrompt myPromptTheme)

  -- M-w, [w]indow management
  , ("M-w x", kill1)
  , ("M-w r", refresh)  -- Resize viewed windows to the correct size
  , ("M-w k", windows W.swapUp)
  , ("M-w j", windows W.swapDown)
  , ("M-w t", withFocused $ windows . W.sink)
  , ("M-w m", windows W.swapMaster)
  , ("M-w b", withFocused toggleBorder)
  , ("M-w d", killAllOtherCopies)
  , ("M-w c a", windows copyToAll)
  ] ++
  [ ("M-w c " ++ k, DO.withNthWorkspace copy i)
  | (i, k) <- zip [0..] wsKeys
  ] ++

  -- M-p, [p]rojects
  [ ("M-p x", confirmPrompt hotPromptTheme "kill project" $
      killAll >> removeWorkspace)
  , ("M-p r", renameProjectPrompt myPromptTheme)
  , ("M-p d", changeProjectDirPrompt myPromptTheme)

  -- M-l, [l]ayout management
  , ("M-l k", sendMessage NextLayout)
  , ("M-l h", sendMessage Shrink)
  , ("M-l l", sendMessage Expand)
  , ("M-l w", sendMessage $ IncMasterN 1)
  , ("M-l d", sendMessage $ IncMasterN (-1))

  -- M-m, [m]usic
  , ("M-m p", spawn "mpc toggle")
  , ("<XF86AudioPlay>", spawn "mpc toggle")
  , ("M-m j", spawn "mpc next")
  , ("<XF86AudioNext>", spawn "mpc next")
  , ("M-m k", spawn "mpc prev")
  , ("<XF86AudioPrev>", spawn "mpc prev")

  -- M-d, [d]isplay
  , ("M-d i", spawn "xrandr --output DP1 --off --output eDP1 --auto")
  , ("M-d e", spawn "xrandr --output DP1 --auto --output eDP1 --off")
  , ("M-d m", spawn "xrandr --output DP1 --auto --output eDP1 --auto")
  , ("M-d l", spawn "xrandr --output DP1 --primary --auto --output eDP1 --left-of DP1 --auto")
  , ("M-d r", spawn "xrandr --output DP1 --primary --auto --output eDP1 --right-of DP1 --auto")
  , ("M-d a", spawn "xrandr --output DP1 --primary --auto --output eDP1 --above DP1 --auto")
  , ("M-d b", spawn "xrandr --output DP1 --primary --auto --output eDP1 --below DP1 --auto")

  -- M-p, [p]assword
  -- misc system
  , ("<XF86AudioMute>",        spawn "amixer set Master toggle")
  , ("M-<Up>",                 spawn "amixer set Master 2%+")
  , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 2%+")
  , ("M-<Down>",               spawn "amixer set Master 2%-")
  , ("<XF86AudioLowerVolume>", spawn "amixer set Master 2%-")

  , ("M-<F7>", spawn "sleep 0.2; xset dpms force off")
  , ("M-<F8>", spawn "~/.xmonad/trackpad-toggle.sh")
  , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2")
  , ("<XF86MonBrightnessUp>",   spawn "xbacklight -inc 2")

  , ("<Print>", spawn "scrot")
  , ("C-<Print>", spawn "sleep 0.2; scrot -s")

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

extraKeys =
  -- Fix for dead acute not being in EZConfig
  [ ( (myModMask, xK_dead_acute)
    , removeEmptyWorkspaceAfter $ DO.withNthWorkspace W.greedyView 11)
  , ( (myModMask .|. shiftMask, xK_dead_acute), DO.withNthWorkspace W.shift 11)
  ]

quitXmonad = io (exitWith ExitSuccess)
restartXmonad = spawn "killall dzen2; xmonad --restart"
rebuildXmonad = spawn "killall dzen2; ~/.xmonad/build.sh; xmonad --restart"


    --
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
           onWorkspace "EDT" editorFirst $
           onWorkspace "DOC" readLayout $
           onWorkspace "VID" fullFirst $
           onWorkspace "STM" fullFirst $
           defaultOrder
  where
    -- Layouts
    tiledLayout  = named "Tiled"   $ tiled
    mirrorLayout = named "Mirror"  $ Mirror tiled
    tabbedLayout = named "Tabbed"  $ tabbed shrinkText myTabTheme
    editorLayout = named "Editor"  $ reflectHoriz $ FixedColumn 1 20 164 10
    readLayout   = named "Reading" $ Tall 1 (3/100) (5/6)
    fullLayout   = noBorders Full

    named x      = renamed [Replace x]
    tiled        = Tall 1 (3/100) (1/2)

    -- Orders
    defaultOrder = tiledFirst
    tiledFirst   = tiledLayout  ||| mirrorLayout ||| tabbedLayout |||
                   editorLayout ||| fullLayout
    tabbedFirst  = tabbedLayout ||| editorLayout ||| fullLayout   |||
                   tiledLayout  ||| mirrorLayout
    editorFirst  = editorLayout ||| fullLayout   ||| tiledLayout  |||
                   mirrorLayout ||| tabbedLayout
    fullFirst    = fullLayout   ||| tiledLayout  ||| mirrorLayout |||
                   tabbedLayout ||| editorLayout

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
                        return () >> checkKeymap conf (myKeymap conf)

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
