--
--  File:       xmonad.hs
--  Author:     Juan Pedro Bolivar Puente <raskolnikov@es.gnu.org>
--  Date:       Fri Oct 30 16:55:12 2009
--
--  Xmonad configuration file.
--
--  Tested: nixos-24.05
--

import XMonad
import System.Exit
import System.Posix.Env

import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import qualified Data.Char       as C
import qualified Data.List       as L
import Data.Ratio ((%))

import XMonad.Util.Run

import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition as I

import XMonad.Layout.BoringWindows as B
import XMonad.Layout.CircleEx
import XMonad.Layout.Gaps as G
import XMonad.Layout.Grid
import XMonad.Layout.IM
import XMonad.Layout.Magnifier
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Renamed as R
import XMonad.Layout.Tabbed

import XMonad.Actions.NoBorders
import XMonad.Actions.Navigation2D
import XMonad.Actions.CopyWindow
import XMonad.Prompt
import XMonad.Prompt.RunOrRaise
import XMonad.Prompt.Window
import XMonad.Prompt.Workspace

import XMonad.Util.Cursor

import XMonad.Hooks.EwmhDesktops

import XMonad.Hooks.TaffybarPagerHints (pagerHints)

import Graphics.X11.ExtraTypes.XF86

-- http://lpaste.net/83047
copyWindowToAll :: (Eq s, Eq i, Eq a) => a -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copyWindowToAll w s =
  foldr (copyWindow w) s $ map W.tag (W.workspaces s)

doSticky :: ManageHook
doSticky =
  do
    win <- ask
    doF $ copyWindowToAll win

doBoring :: ManageHook
doBoring =
  do
    win <- ask
    liftX (broadcastMessage $ B.Merge "managed" [win])
    doF id

altMask = mod1Mask

main :: IO ()
main = do
  let backgroundColor = "#44475A"
      headerColor     = "#000000"
      focusedColor    = "#BD93F9"
      textColor       = "#ddd"
      textFont        = "Cantarell-20:bold"
      xpConfig        = def
        { font              = "xft:" ++ textFont
        , bgColor           = headerColor
        , fgColor           = textColor
        , bgHLight          = focusedColor
        , fgHLight          = headerColor
        , borderColor       = backgroundColor
        , autoComplete      = Nothing
        , promptBorderWidth = 0
        , height            = 26
        , alwaysHighlight   = True
        , searchPredicate   = L.isInfixOf . map C.toLower
        }
      dmenuCmd = "~/usr/bin/dmenu-run-xft -i -b "
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
        , ((mask .|. shiftMask, xK_g), windowPrompt def Goto wsWindows)
        , ((mask .|. shiftMask, xK_b), windowPrompt def Bring allWindows)
        , ((mask, xK_i), workspacePrompt xpConfig (windows . W.view))
        -- close focused window
        , ((mask, xK_c), kill)
         -- Rotate through the available layout algorithms
        , ((mask, xK_0), sendMessage NextLayout)
        --  Reset the layouts on the current workspace to default
        , ((mask, xK_9), setLayout $ XMonad.layoutHook conf)
        -- Resize viewed windows to the correct size
        , ((mask .|. shiftMask, xK_n), refresh)
        -- Move focus to the next window
        , ((mask, xK_Tab), B.focusDown)
        -- Move focus to the next window
        , ((mask, xK_k), B.focusDown)
        -- Move focus to the previous window
        , ((mask, xK_j), B.focusUp)
        -- Move focus to the master window
        , ((mask, xK_m), B.focusMaster)
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
        -- Toggle trackpad
        , ((mask .|. shiftMask, xK_t), spawn $ "~/usr/bin/touchpad-toggle")
        -- Increment the number of windows in the master area
        , ((mask, xK_comma), sendMessage (IncMasterN 1))
        -- Deincrement the number of windows in the master area
        , ((mask, xK_period), sendMessage (IncMasterN (-1)))
        -- toggle the status bar gap
        , ((mask, xK_b), sendMessage ToggleStruts)
        -- , ((mask, xK_b), sendMessage $ G.ToggleGaps)
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
        -- , ((noModMask, xF86XK_AudioNext), spawn $ "mpc next")
        -- , ((noModMask, xF86XK_AudioPrev), spawn $ "mpc prev")
        -- , ((noModMask, xF86XK_AudioPlay), spawn $ "mpc toggle")
        -- , ((noModMask, xF86XK_AudioStop), spawn $ "mpc stop")
        , ((noModMask, xF86XK_AudioLowerVolume), spawn $ "pamixer -d 5")
        , ((noModMask, xF86XK_AudioRaiseVolume), spawn $ "pamixer -i 5")
        -- Nautilus
        , ((mask, xK_n), spawn $ "nautilus")
        , ((mask .|. shiftMask, xK_n), spawn $ "nautilus -w")
        -- Emacs
        , ((mask, xK_e), spawn $ "wmctrl -xa emacs || emacsclient -c -e '(ignore)'")
        , ((mask .|. shiftMask, xK_e), spawn $ "emacsclient -c -e '(ignore)'")
        -- Browser
        , ((mask, xK_w), spawn $ "wmctrl -xa firefox || firefox")
        , ((mask .|. shiftMask, xK_w), spawn $ "firefox")
        -- take a screenshot of entire display
        , ((noModMask, xK_Print),        spawn "xfce4-screenshooter -f -s ~/pic")
        , ((shiftMask, xK_Print),        spawn "xfce4-screenshooter -r -s ~/pic")
        , ((mask,      xK_Print),        spawn "xfce4-screenshooter")
        -- open configuration manels
        , ((noModMask, xF86XK_Tools),    spawn "xfce4-settings-manager")
        , ((shiftMask, xF86XK_Tools),    spawn "gnome-control-center")
        , ((mask,      xF86XK_Tools),    spawn "gnome-tweak-tool")
        , ((noModMask, xF86XK_Explorer), spawn "xfce4-display-settings -m")
        , ((mask .|. shiftMask, xK_m),   spawn "pavucontrol")
        , ((mask,      xK_m),            spawn "cantata")
        -- Directional navigation of windows
        -- , ((mask, xK_Right), windowGo R False)
        -- , ((mask, xK_Left ), windowGo L False)
        -- , ((mask, xK_Up   ), windowGo U False)
        -- , ((mask, xK_Down ), windowGo D False)
        -- Swap adjacent windows
        -- , ((mask .|. shiftMask, xK_Right), windowSwap R False)
        -- , ((mask .|. shiftMask, xK_Left ), windowSwap L False)
        -- , ((mask .|. shiftMask, xK_Up   ), windowSwap U False)
        -- , ((mask .|. shiftMask, xK_Down ), windowSwap D False)
        -- Maximize
        -- , ((mask .|. shiftMask, xK_minus ), sendMessage MagnifyMore)
        -- , ((mask, xK_minus), sendMessage MagnifyLess)
        -- , ((mask, xK_o    ), sendMessage Toggle)
        -- , ((mask, xK_backslash), withFocused (sendMessage . maximizeRestore))
        -- , ((mask, xK_plus), withFocused (sendMessage . maximize))
        -- , ((mask,               xK_o), withFocused minimizeWindow)
        -- , ((mask .|. shiftMask, xK_o), sendMessage RestoreNextMinimizedWin)
        ]
        ++
        -- Move workspace
        [((m .|. mask, k), windows $ f i)
            | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
            , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
        ++
        -- mod-{a,s,d} %! Switch to physical/Xinerama screens 1, 2, or 3
        -- mod-shift-{a,s,d} %! Move client to screen 1, 2, or 3
        [((m .|. mask, key), screenWorkspace sc >>= flip whenJust (windows . f))
            | (key, sc) <- zip [xK_a, xK_s, xK_d] [0..]
            , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

  let mouseBindings' (XConfig {XMonad.modMask = mask}) = M.fromList $
        -- mod-button1, Set the window to floating mode and move by dragging
        [ ((mask, button1), (\w -> focus w >> mouseMoveWindow w))
        -- mod-button2, Raise the window to the top of the stack
        , ((mask, button2), (\w -> focus w >> windows W.swapMaster))
        -- mod-button3, Set the window to floating mode and resize by dragging
        , ((mask, button3), (\w -> focus w >> mouseResizeWindow w))
        ]

  let layout' = avoidStruts $ smartBorders $ B.boringWindows $ normalLayout
        where
          -- gap = G.gaps [(G.U, 100)]
          magnify      = id -- magnifiercz' (100/80)
          tallLayout   = R.renamed [ R.Replace "tall" ] $ minimize $ magnify $ Tall 1 (3/100) (6/10)
          circleLayout = R.renamed [ R.Replace "circle" ] $ minimize $ magnify circleEx
          tabbedLayout = R.renamed [ R.Replace "tabbed" ] $ minimize $ magnify simpleTabbed
          fullLayout   = R.renamed [ R.Replace "full" ] $ minimize $ Full
          imLayout     = R.renamed [ R.CutWordsLeft 2 ] $ magnifiercz' (100/80) $ withIM (2%10)
                         (Or (Role "buddy_list") (Title "magnicida - Skype™"))
                         (circleLayout ||| tallLayout ||| fullLayout)
          normalLayout = circleLayout ||| tallLayout ||| fullLayout

  let manageHook' = composeAll
        [ resource  =? "Do"              --> doIgnore
        , className =? "stalonetray"     --> doIgnore
        , className =? "trayer"          --> doIgnore
        , className =? "Xfdesktop"       --> doIgnore
        , className =? "xfdesktop"       --> doIgnore
        , title     =? "Desktop"         --> doIgnore
        , className =? "kruler"          --> doIgnore

        , className =? "Cinelerra"               --> doCenterFloat
        , className =? "sun-applet-Main"         --> doCenterFloat
        , resource  =? "sun-awt-X11-XDialogPeer" --> doCenterFloat
        , resource  =? "javax.swing.JDialog"     --> doCenterFloat
        , className =? "Tgcm"                    --> doCenterFloat
        , className =? "QjackCtl"                --> doSideFloat SE
        , className =? "Qjackctl"                --> doSideFloat SE
        , className =? "qjackctl"                --> doSideFloat SE
        , className =? "Qjackctl.real"           --> doSideFloat SE
        , className =? "Gcr-prompter"            --> doCenterFloat
        , className =? "Emoji-keyboard"          --> doCenterFloat
        , className =? "Pavucontrol"             --> (doRectFloat $ W.RationalRect (1 % 4) (1 % 4) (1 % 2) (1 % 2))
        , className =? "Screenkey"               --> (doRectFloat $ W.RationalRect 0.7 0.8 0.3 0.13)
        , className =? "SimpleScreenRecorder"    --> doSideFloat SE
        , className =? "kmag"                    --> doSideFloat SE

        , className =? "Icedove-bin"       --> doShift "mail"
        , className =? "Icedove"           --> doShift "mail"
        , className =? "Pidgin"            --> doShift "chat"
        , className =? "Org.gnome.Polari"  --> doShift "chat"
        , className =? "Skype"             --> doShift "chat"
        , className =? "Slack"             --> doShift "chat"
        , className =? "discord"           --> doShift "chat"
        , className =? "TelegramDesktop"   --> doShift "chat"
        , className =? "telegram"          --> doShift "chat"
        , className =? "KotatogramDesktop" --> doShift "chat"

        , title     =? "SoundX AI"       --> doCenterFloat

        , isFullscreen --> doFullFloat
        , checkDialog  --> doCenterFloat
        , checkMenu    --> doCenterFloat
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

  putEnv "GTK_CSD=0"
  putEnv "_JAVA_AWT_WM_NONREPARENTING=1"
  putEnv "QT_AUTO_SCREEN_SCALE_FACTOR=0"
  putEnv "XDG_CURRENT_DESKTOP=gnome"
  -- putEnv "QT_STYLE_OVERRIDE=breeze"
  -- putEnv "QT_SCALE_FACTOR=1.25"
  -- putEnv "QT_QPA_PLATFORMTHEME=lxqt"
  -- putEnv "GTK_THEME=adw-gtk3"
  putEnv "GPODDER_HOME=/home/raskolnikov/sync/gpodder"
  spawn "~/usr/bin/xmonad-session-script"
  xmonad $ ewmhFullscreen . ewmh $ pagerHints $ withUrgencyHook NoUrgencyHook $ docks $ def
    { terminal           = terminalCmd
    , focusFollowsMouse  = True
    , borderWidth        = 4
    , modMask            = mod4Mask
    , workspaces         = [ "web", "emacs", "misc", "chat" ]
    , normalBorderColor  = headerColor --"#242424" --backgroundColor
    , focusedBorderColor = backgroundColor --focusedColor
    , keys               = keys'
    , mouseBindings      = mouseBindings'
    , manageHook         = manageHook' <+> manageDocks
    , layoutHook         = layout'
    , startupHook        = setDefaultCursor xC_arrow
    , handleEventHook    = handleEventHook def
    , logHook            = do
        spawn "~/usr/bin/xdotool-all Dunst windowraise"
        spawn "~/usr/bin/xdotool-all Taffybar-linux-x86_64 windowlower"
    }
