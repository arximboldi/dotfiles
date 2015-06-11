--
--  File:       xmonad.hs
--  Author:     Juan Pedro Bolivar Puente <raskolnikov@es.gnu.org>
--  Date:       Fri Oct 30 16:55:12 2009
--
--  Xmonad configuration file.
--

import XMonad
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.Char       as C
import qualified Data.List       as L

import XMonad.Util.Run

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook

import XMonad.Layout.NoBorders
import XMonad.Layout.Circle
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed
import XMonad.Actions.NoBorders
import XMonad.Actions.Navigation2D
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window

import XMonad.Util.Cursor

import XMonad.Hooks.EwmhDesktops
import System.Taffybar.Hooks.PagerHints

main :: IO ()
main = do
  let backgroundColor = "#2d2d2d"
      headerColor     = "#1d1d1d"
      focusedColor    = "#D64937"
      textColor       = "#bbbbbb"
      textFont        = "Sans-9:bold"
      xpConfig        = defaultXPConfig
        { font              = "xft:" ++ textFont
        , bgColor           = headerColor
        , fgColor           = textColor
        , bgHLight          = focusedColor
        , fgHLight          = headerColor
        , borderColor       = backgroundColor
        , autoComplete      = Nothing
        , promptBorderWidth = 0
        , height            = 22
        , alwaysHighlight   = True
        , searchPredicate   = L.isInfixOf . map C.toLower
        }
      dmenuCmd = "~/usr/bin/dmenu_run_xft -b "
                 ++ "  -fn '" ++ textFont
                 ++ "' -nb '" ++ headerColor
                 ++ "' -nf '" ++ textColor
                 ++ "' -sb '" ++ focusedColor
                 ++ "' -sf '" ++ headerColor
                 ++ "'"
      terminalCmd = "gnome-terminal --hide-menubar"

  let keys' conf@(XConfig {XMonad.modMask = mask}) = M.fromList $
        -- launch a terminal
        [ ((mask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)
        -- launch dmenu
        , ((mask, xK_space), spawn dmenuCmd)
        , ((mask, xK_p), spawn dmenuCmd)
        , ((mask .|. shiftMask, xK_p), runOrRaisePrompt xpConfig)
        , ((mask .|. shiftMask, xK_g), windowPromptGoto  xpConfig)
        , ((mask .|. shiftMask, xK_b), windowPromptBring xpConfig)
        -- , ((mask, xK_w), workspacePrompt xpConfig (windows . W.view))
              -- Directional navigation of windows
        , ((mask, xK_Right), windowGo R False)
        , ((mask, xK_Left ), windowGo L False)
        , ((mask, xK_Up   ), windowGo U False)
        , ((mask, xK_Down ), windowGo D False)
          -- Swap adjacent windows
        , ((mask .|. shiftMask, xK_Right), windowSwap R False)
        , ((mask .|. shiftMask, xK_Left ), windowSwap L False)
        , ((mask .|. shiftMask, xK_Up   ), windowSwap U False)
        , ((mask .|. shiftMask, xK_Down ), windowSwap D False)
        -- close focused window
        , ((mask, xK_c), kill)
         -- Rotate through the available layout algorithms
        , ((mask, xK_0), sendMessage NextLayout)
        --  Reset the layouts on the current workspace to default
        , ((mask, xK_9), setLayout $ XMonad.layoutHook conf)
        -- Resize viewed windows to the correct size
        , ((mask .|. shiftMask, xK_n), refresh)
        -- Move focus to the next window
        , ((mask, xK_Tab), windows W.focusDown)
        -- Move focus to the next window
        , ((mask, xK_k), windows W.focusDown)
        -- Move focus to the previous window
        , ((mask, xK_j), windows W.focusUp)
        -- Move focus to the master window
        , ((mask, xK_m), windows W.focusMaster)
        -- Toggle border
        , ((mask,  xK_g ), withFocused toggleBorder)
        -- Swap the focused window and the master window
        , ((mask, xK_Return), windows W.swapMaster)
        -- Swap the focused window with the next window
        , ((mask .|. shiftMask, xK_k), windows W.swapDown)
        -- Swap the focused window with the previous window
        , ((mask .|. shiftMask, xK_j), windows W.swapUp)
        -- Shrink the master area
        , ((mask, xK_h), sendMessage Shrink)
        -- Expand the master area
        , ((mask, xK_l), sendMessage Expand)
        -- Push window back into tiling
        , ((mask, xK_t), withFocused $ windows . W.sink)
        -- Increment the number of windows in the master area
        , ((mask, xK_comma), sendMessage (IncMasterN 1))
        -- Deincrement the number of windows in the master area
        , ((mask, xK_period), sendMessage (IncMasterN (-1)))
        -- toggle the status bar gap
        , ((mask, xK_b), sendMessage ToggleStruts)
        -- Quit xmonad
        , ((mask .|. shiftMask, xK_q), io (exitWith ExitSuccess))
        -- Restart xmonad
        , ((mask, xK_q), restart "xmonad" True)
        -- Lock screen
        , ((mask .|. shiftMask, xK_ntilde), spawn $ "gnome-screensaver-command -lock")
        , ((mask .|. shiftMask, xK_comma),  spawn $ "sleep 1;xset dpms force off")
        -- GMPC
        , ((mask, xK_Page_Down), spawn $ "mpc next")
        , ((mask, xK_Page_Up),   spawn $ "mpc prev")
        , ((mask, xK_Home),      spawn $ "mpc toggle")
        , ((mask, xK_End),       spawn $ "mpc stop")
        -- Nautilus
        , ((mask, xK_n), spawn $ "nautilus")
        -- take a screenshot of entire display
        , ((noModMask, xK_Print), spawn "gnome-screenshot")
        , ((shiftMask, xK_Print), spawn "gnome-screenshot -w -B")
        , ((mask, xK_Print), spawn "gnome-screenshot -i")
        -- Maximize
        , ((mask .|. shiftMask, xK_plus ), sendMessage MagnifyMore)
        , ((mask .|. shiftMask, xK_minus), sendMessage MagnifyLess)
        , ((mask              , xK_o    ), sendMessage Toggle)
        -- , ((mask, xK_backslash), withFocused (sendMessage . maximizeRestore))
        -- , ((mask, xK_plus), withFocused (sendMessage . maximize))
        ]
        ++
        -- Move workspace
        [((m .|. mask, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

  let mouseBindings' (XConfig {XMonad.modMask = mask}) = M.fromList $
        -- mod-button1, Set the window to floating mode and move by dragging
        [ ((mask, button1), (\w -> focus w >> mouseMoveWindow w))
        -- mod-button2, Raise the window to the top of the stack
        , ((mask, button2), (\w -> focus w >> windows W.swapMaster))
        -- mod-button3, Set the window to floating mode and resize by dragging
        , ((mask, button3), (\w -> focus w >> mouseResizeWindow w))
        ]

  let layout' = smartBorders normalLayout
        where
          tallLayout  = Tall 1 (3/100) (6/10)
          circleLayout = renamed [ Replace "Circle" ] $ magnifiercz' (100/80) Circle
          normalLayout = circleLayout ||| tallLayout ||| Full

  let manageHook' = composeAll
        [ resource    =? "Do"            --> doIgnore
        , className =? "stalonetray"     --> doIgnore
        , className =? "trayer"          --> doIgnore

        , className =? "Cinelerra"               --> doFloat
        , className =? "sun-applet-Main"         --> doFloat
        , resource  =? "sun-awt-X11-XDialogPeer" --> doFloat
        , resource  =? "javax.swing.JDialog"     --> doFloat
        , className =? "Tgcm"                    --> doFloat
        , className =? "Qjackctl"                --> doFloat
        , className =? "Qjackctl.real"           --> doFloat
        , className =? "Mixxx"                   --> doFloat

        , className =? "Icedove-bin"      --> doShift "mail"
        , className =? "Icedove"          --> doShift "mail"
        , className =? "Emacs"            --> doShift "emacs"

        , isFullscreen --> doFullFloat
        , checkDialog --> doFloat
        , checkMenu   --> doFloat
        ]
        where
          -- http://bbs.archlinux.org/viewtopic.php?id=74839
          getProp a w = withDisplay $ \dpy -> io $ getWindowProperty32 dpy a w
          checkAtom name value = ask >>= \w -> liftX $ do
            a <- getAtom name
            val <- getAtom value
            mbr <- getProp a w
            case mbr of
              Just [r] -> return $ elem (fromIntegral r) [val]
              _ -> return False
          checkDialog = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_DIALOG"
          checkMenu   = checkAtom "_NET_WM_WINDOW_TYPE" "_NET_WM_WINDOW_TYPE_MENU"

  runProcessWithInput "killall" ["nm-applet"] ""
  runProcessWithInput "killall" ["taffybar-linux-x86_64"] ""
  runProcessWithInput "killall" ["mpDris", "-9"] ""
  spawnPipe "nitrogen --restore"
  spawnPipe "xsettingsd"
  spawnPipe "synclient TapButton1=1"
  spawnPipe "mpDris"
  spawnPipe "xfce4-power-manager --restart"
  spawnPipe "xfce4-volumed-pulse --no-daemon"
  spawnPipe "tracker-control --start"
  spawnPipe "nautilus --no-default-window"
  spawnPipe "volti"
  spawnPipe "nm-applet"
  spawnPipe "ibus-daemon --replace"
  spawnPipe "sleep 1 && taffybar"

  xmonad $ ewmh $ pagerHints $ withUrgencyHook NoUrgencyHook $ withNavigation2DConfig defaultNavigation2DConfig $ defaultConfig
    { terminal           = terminalCmd
    , focusFollowsMouse  = True
    , borderWidth        = 4
    , modMask            = mod4Mask
    , workspaces         = [ "web", "emacs", "misc", "mail" ]
    , normalBorderColor  = backgroundColor
    , focusedBorderColor = focusedColor
    , keys               = keys'
    , mouseBindings      = mouseBindings'
    , manageHook         = manageHook' <+> manageDocks
    , layoutHook         = avoidStruts $ layout'
    , startupHook        = setDefaultCursor xC_arrow
    }
