--
-- File     : ~/.xmonad/xmonad.hs (for Xmonad >= 0.9)
-- Author   : Thayer Williams
-- Website  : http://cinderwick.ca/
-- Desc     : A simple, mouse-friendly xmonad config geared towards
--            netbooks and other low-resolution devices.
--
--            dzen is used for statusbar rendering, with optional mouse
--            integration provided by xdotool:
--
--             * left-click workspace num to go to that ws
--             * left-click layout to cycle next layout
--             * left-click window title to cycle next window
--             * middle-click window title to kill focused window
--

import Data.Char (toLower)
import Data.Ratio ((%))
import System.IO                   -- hPutStrLn scope
import Data.List (intercalate, intersperse, isSuffixOf, isPrefixOf, elemIndex)
import Data.Maybe (fromMaybe)

import qualified Network.MPD as MPD
import qualified Network.MPD.Commands.Extensions as MPD

import XMonad
import XMonad.Actions.CycleWindows -- classic alt-tab
import XMonad.Actions.CycleWS      -- cycle thru WS', toggle last WS
import XMonad.Actions.GridSelect

import XMonad.Hooks.DynamicLog     -- statusbar
import XMonad.Hooks.EwmhDesktops   -- fullscreenEventHook fixes chrome fullscreen
import XMonad.Hooks.ManageDocks    -- dock/tray mgmt
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook    -- window alert bells

import XMonad.Layout.Named         -- custom layout names
import XMonad.Layout.NoBorders     -- smart borders on solo clients
import XMonad.Layout.PerWorkspace
import XMonad.Layout.IM

import XMonad.Util.MPD
import XMonad.Util.NamedWindows (getName)
import XMonad.Util.EZConfig        -- append key/mouse bindings
import XMonad.Util.Run(spawnPipe)  -- spawnPipe and hPutStrLn
import XMonad.Util.WorkspaceCompare (WorkspaceCompare, WorkspaceSort, mkWsSort, getWsIndex)
import XMonad.Util.NamedScratchpad
import XMonad.Util.MPD
import XMonad.Util.Cursor
import XMonad.Util.Loggers

import qualified XMonad.StackSet as W   -- manageHook rules

main = do
        status <- spawnPipe myDzenStatus    -- xmonad status on the left
        conky  <- spawnPipe myDzenConky     -- conky stats on the right
        xmonad $ withUrgencyHook LibNotifyUrgencyHook $ defaultConfig
            { modMask            = mod4Mask
            , terminal           = "urxvt"
            , borderWidth        = 2
            , normalBorderColor  = "#dddddd"
            , focusedBorderColor = "#0000ff"
            , handleEventHook    = fullscreenEventHook
            , workspaces = myWorkspaces
            , layoutHook = myLayoutHook
            , startupHook = myStartupHook
            , manageHook = manageDocks  <+> myManageHook <+> manageHook defaultConfig
            , logHook    = myLogHook status
            }
            `additionalKeysP` myKeys

data LibNotifyUrgencyHook = LibNotifyUrgencyHook deriving (Read, Show)

instance UrgencyHook LibNotifyUrgencyHook where
    urgencyHook LibNotifyUrgencyHook w = do
        name <- getName w
        ws <- gets windowset
        c <- withDisplay $ \d -> fmap resClass $ io $ getClassHint d w
        whenJust (W.findTag w ws) (flash name c)
      where flash _ "Pidgin" _ = spawn "true"
            flash _ "Gajim" _  = spawn "true"
            flash name c index = spawn $
                                 intercalate " " $
                                 [ "notify-send -i"
                                 , icon
                                 , show $ show name
                                 , show $ "on " ++ index ]
                where icon = case c of
                               "URxvt" -> "gnome-terminal"
                               otherwise -> map toLower c

focusUrgent' :: X ()
focusUrgent' = withUrgents $ (windows . focusOrSwitch)
               where
                   focusOrSwitch (w:_) = W.focusWindow w
                   focusOrSwitch _ = W.greedyView =<< W.tag . head . namedScratchpadFilterOutWorkspace . W.hidden

-- Tags/Workspaces
myWorkspaces :: [WorkspaceId]
myWorkspaces  = ["1:web","2:dev","3:im","4:doc","5:mail","s:fm", "7:cad"]

-- Layouts
-- the default layout is fullscreen with smartborders applied to all
myLayoutHook = avoidStruts $
    onWorkspace "3:im" imLayout $
    onWorkspaces ["1:web", "2:dev"] ( full ||| mtiled ||| tiled ) $
    smartBorders $
    standardLayouts
  where
    tall    = Tall 1           0.02        0.5
    full    = named "X" $ Full
    mtiled  = named "M" $ Mirror tiled
    tiled   = named "T" $ Tall 1 (5/100) (2/(1+(toRational(sqrt(5)::Double))))
    standardLayouts = tall ||| Mirror tall ||| Full
    imLayout = withIM (1%4) (Or (And (Role "roster") (ClassName "Gajim"))
                                (Title "mic92_ - Skype™") ) (Full)

-- Window management
myManageHook :: ManageHook
myManageHook = composeAll . concat $
   [ [ isFullscreen  --> doFullFloat ]
   , [ isDialog --> doF W.shiftMaster          <+>     doF W.swapDown ]
   , [ className =? c --> doCenterFloat         | c <- myFloatClasses ]
   , [ className =? c --> doShiftAndGo "1:web"  | c <- ["Firefox", "Chromium", "Opera", "Aurora"]]
   , [ className =? c --> doShiftAndGo "2:dev"  | c <- ["URxvt"]]
   , [ className =? c --> doShift "3:im"        | c <- ["Gajim", "Skype"] ]
   , [ className =? c --> doShiftAndGo "4:doc"  | c <- ["Keepassx", "evince"] ]
   , [ className =? c --> doShift "5:mail"      | c <- ["Thunderbird"] ]
   , [ className =? c --> doShift "s:fm"        | c <- ["Stuurman", "cdsLibManager"] ]
   , [ className =? c --> doShiftAndGo "7:cad"  | c <- cadence_windows ++ ["Opus"] ]
   , [ className =? c --> doIgnore              | c <- ["trayer","Xfce4-notifyd"] ]
    ]
    where myFloatClasses = [ "MPlayer", "feh", "Xmessage", "Grip", "DClock"
                           , "Gimp", "XDosEmu", "Nitrogen", "Nvidia-settings"
                           , "VirtualBox", "Sonata", "xine", "Blender:Render"
                           , "Vdesk.py", "org-scilab-modules-jvm-Scilab"
                           , "qemu-system-x86_64", "sun-applet-Main"]
          doShiftAndGo ws = doF (W.greedyView ws) <+> doShift ws
          cadence_windows = [ "Cadence Window " ++ show(w) | w <- [1..10]]
myStartupHook = setDefaultCursor xC_left_ptr

-- Statusbar
myLogHook h = dynamicLogWithPP $ dzenPP {
    ppOutput  = \str -> hPutStrLn h ("^fg(#3399ff)^i(.xmonad/dzen-icons/arch_10x10.xbm)^fg()"++ str)
  , ppCurrent = dzenColor "#3399ff" "" . wrap " " " " . clickable
  , ppHidden  = dzenColor "#dddddd" "" . wrap " " " " . clickable
  , ppExtras = [ current_date ]
  , ppHiddenNoWindows = const ""
  , ppUrgent  = dzenColor "#ff0000" "" . wrap " " " "
  , ppSep     = "     "
  , ppLayout  = dzenColor "#aaaaaa" "" . wrap "^ca(1,xdotool key super+space)· " " ·^ca()"
  , ppTitle   = dzenColor "#ffffff" ""
        . wrap "^ca(1,xdotool key super+k)^ca(2,xdotool key super+shift+c)"
        "                          ^ca()^ca()" . shorten 30 . dzenEscape
} where
    cal = dzenEscape "show_cal"
    -- clickable workspaces via dzen/xdotool
    current_date = wrapL ("^ca(1,show)") "^ca()" (date "%a %d %b - %R")
    clickable ws = "^ca(1,xdotool key super+" ++ workspaceIndex(ws) ++ ")" ++ ws ++ "^ca()"
    workspaceIndex key = show (fromMaybe 0 (elemIndex key myWorkspaces) + 1)

myDzenStatus = "dzen2 -ta 'l' -x '0' -y '0'" ++ myDzenStyle
myDzenConky  = "conky -c ~/.xmonad/conkyrc | dzen2 -y '1200' -ta 'l'" ++ myDzenStyle
myDzenStyle  = " -e 'onstart=lower' -dock -h '15' -fg '#777777' -bg '#222222' -fn 'arial:bold:size=10'"

-- Key bindings
--
restart_cmd = "killall dzen2; killall conky; cd ~/.xmonad; ghc -threaded xmonad.hs; mv xmonad xmonad-x86_64-linux;" ++
    "xmonad --restart; systemctl --user restart trayer"
myKeys = [ ("M-b"        , sendMessage ToggleStruts                 )     -- toggle the status bar gap
         , ("M1-<Tab>"   , cycleRecentWindows [xK_Alt_L] xK_Tab xK_Tab )  -- classic alt-tab behaviour
         , ("M-<Return>" , spawn "urxvt"                            )     -- swap the focused window and the master window
         , ("M-<Tab>"    , toggleWS                                 )     -- toggle last workspace (super-tab)
         , ("M-<Right>"  , nextWS                                   )     -- go to next workspace
         , ("M-<Space>"  , sendMessage NextLayout                   )
         , ("M-<Left>"   , prevWS                                   )     -- go to prev workspace
         , ("M-S-<Right>", shiftToNext                              )     -- move client to next workspace
         , ("M-S-<Left>" , shiftToPrev                              )     -- move client to prev workspace
         , ("M-u"        , focusUrgent                              )     -- focus urgent
         , ("M-g", goToSelected defaultGSConfig)
         , ("M-c"        , spawn "gnome-calculator"                 )     -- calc
         , ("M-p"        , spawn "valauncher"                       )     -- app launcher
         , ("M-r"        , spawn restart_cmd                        )     -- restart xmonad
         , ("M-w"        , spawn "firefox"                          )     -- launch browser
         , ("M-s"        , spawn "stuurman"                         )     -- launch file manager
         , ("C-M1-l"     , spawn "slimlock"                         )     -- lock screen
         , ("C-M1-<Delete>" , spawn "systemctl reboot"              )     -- reboot
         , ("C-M1-<Insert>" , spawn "systemctl poweroff"            )     -- poweroff
         , ("<XF86AudioRaiseVolume>", spawn "amixer sset Master 3%+")
         , ("<XF86AudioLowerVolume>", spawn "amixer sset Master 3%-")
         , ("<XF86AudioMute>", spawn "amixer sset Master toggle"    )
         , ("<XF86AudioPlay>", withMPD MPD.toggle                   )
         , ("S-<Space>", withMPD MPD.toggle                         )
         , ("<XF86AudioStop>", withMPD MPD.stop                     )
         , ("<XF86AudioPrev>", withMPD MPD.previous                 )
         , ("<XF86AudioNext>", withMPD MPD.next                     )
         , ("<XF86WLAN>", spawn ""                                  )
         , ("<Print>", spawn "scrot %Y-%m-%d-%T.png --exec 'eog \"$f\"; mv \"$f\" /home/joerg/upload'")
         , ("M-<Print>", spawn "scrot %Y-%m-%d-%T.png --select --exec 'eog \"$f\"; mv \"$f\" /home/joerg/upload'")
         ]

-- vim:sw=4 sts=4 ts=4 tw=0 et ai
