-- core
import XMonad

-- window stack manipulation and map creation
import Data.Tree
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.Maybe (fromJust)

-- system
import System.Exit (exitSuccess)
import System.IO (hPutStrLn)

-- hooks
import XMonad.Hooks.ManageDocks(avoidStruts, docks, manageDocks, ToggleStruts(..))
import XMonad.Hooks.DynamicLog(dynamicLogWithPP, wrap, xmobarPP, xmobarColor, shorten, PP(..))
import XMonad.Hooks.EwmhDesktops

-- layout
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.Spacing
import XMonad.Layout.ResizableTile
import XMonad.Layout.LayoutModifier(ModifiedLayout)

-- actions
import XMonad.Actions.CopyWindow(copy, kill1, copyToAll, killAllOtherCopies)
import XMonad.Actions.Submap(submap)

-- utils
import XMonad.Util.Run (spawnPipe)

--import XMonad.Util.SpawnOnce
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce

-- keys
import Graphics.X11.ExtraTypes.XF86

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--colors
data ColorSchemes = ColorSchemes{black ,white ,gray ,yellow ,orange ,red ,purple ,blue ,cyan ,green :: String}

myGruvbox :: ColorSchemes
myGruvbox = ColorSchemes {
                          black   = "#282828",
                          white   = "#ebdbb2",
                          gray    = "#928374",
                          yellow  = "#fabd2f",
                          orange  = "#fe8019",
                          red     = "#fb4934",
                          purple  = "#d3869b",
                          blue    = "#83a598",
                          cyan    = "#8ec07c",
                          green   = "#b8bb26"
                         }

mySolarized :: ColorSchemes
mySolarized = ColorSchemes {
                            black   = "#002b36",
                            white   = "#eee8d5",
                            gray    = "#073642",
                            yellow  = "#b58900",
                            orange  = "#cb4b16",
                            red     = "#d30102",
                            purple  = "#d33682",
                            blue    = "#268bd2",
                            cyan    = "#2aa198",
                            green   = "#859900"
                           }


-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- user variables
myModMask              = mod4Mask                                                                 :: KeyMask
myFocusFollowsMouse    = True                                                                     :: Bool
myClickJustFocuses     = False                                                                    :: Bool
myBorderWidth          = 2                                                                        :: Dimension
myWindowGap            = 0                                                                        :: Integer
myColor                = mySolarized                                                              :: ColorSchemes
myFocusedBorderColor   = white myColor                                                            :: String
myUnFocusedBorderColor = black myColor                                                            :: String
myFont                 = "xft:Hack Nerd Font:regular:size=12:antialias=true:hinting=true"         :: String
myTerminal             = "st"                                                                  :: String
myTerminalAlt          = "emacsclient -c -a '' --eval '(eshell nil)'"                             :: String
myFilemanager          = "emacsclient -c -a '' --eval '(dired nil)'"                              :: String
myFilemanagerAlt       = "pcmanfm"                                                                :: String
myBrowser              = "firefox" :: String
myBrowserAlt           = "brave --profile-directory='Profile 1'" :: String
myMail                 = "emacsclient -c -a '' --eval '(mu4e)'"                                   :: String
myMusicplayer          = myTerminal ++ " -e ncmpcpp"                                              :: String
myRssreader            = "emacsclient -c -a '' --eval '(elfeed)'"                                 :: String
myIDE                  = "emacsclient -c -a emacs"                                                :: String

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Layouts
mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

tall =
  renamed [Replace "Tall"] $
    mySpacing myWindowGap $
        ResizableTall 1 (3/100) (1/2) []

wide =
  renamed [Replace "Wide"] $
    mySpacing myWindowGap $
        Mirror (Tall 1 (3 / 100) (1 / 2))

full =
  renamed [Replace "Full"] $
    mySpacing myWindowGap $
        Full

myLayout =
  --avoidStruts $ smartBorders myDefaultLayout
  smartBorders myDefaultLayout
  where
    myDefaultLayout = full ||| tall

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Prompts
myPromptList :: [(KeySym, String)]
myPromptList = [(xK_p, "dmenu_power.sh"),
                (xK_k, "dmenu_kill.sh"),
                (xK_m, "dmenu_man.sh")
                  ]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- Scratchpad
myScratchPads =
  [NS "htop" spawnHtop findHtop manageHtop,
    NS "term" spawnTerm findTerm manageTerm,
    NS "connman" spawnConnman findConnman manageConnman
  ]
  where
    spawnHtop = myTerminal ++ " -e htop"
    findHtop = title =? "htop"
    manageHtop = customFloating $ W.RationalRect l t w h
      where
        h = 0.90
        w = 0.95
        t = (1 - h) / 2
        l = (1 - w) / 2
    spawnTerm = "st -T st-nsp"
    findTerm = title =? "st-nsp"
    manageTerm = customFloating $ W.RationalRect l t w h
      where
        h = 0.90
        w = 0.95
        t = (1 - h) / 2
        l = (1 - w) / 2
    spawnConnman = "connman-gtk --class Connman-gtk-nsp"
    findConnman = className =? "Connman-gtk-nsp"
    manageConnman = customFloating $ W.RationalRect l t w h
      where
        h = 0.90
        w = 0.95
        t = (1 - h) / 2
        l = (1 - w) / 2

myScratchPadList :: [(KeySym, String)]
myScratchPadList = [(xK_h, "htop"),
                    (xK_t, "term"),
                    (xK_c, "connman")
                   ]

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--managehook
myManageHook =
  composeAll
    [ manageDocks,
      className =? "Steam" --> doFloat,
      className =? "Pavucontrol" --> doFloat,
      className =? "mpv" --> doFloat,
      title     =? "Picture in Picture" --> doFloat,
      className =? "Freetube" --> doFloat,
      className =? "VirtualBox Manager" --> doFloat,
      className =? "Steam" --> doShift (myWorkspaces !! 7),
      className =? "csgo_linux64" --> doShift (myWorkspaces !! 8)
    ]
    <+> namedScratchpadManageHook myScratchPads

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- eventhook
myEventHook = mempty

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--dynamicloghook
windowCount :: X (Maybe String)
windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myXmobarPP h =
  xmobarPP
    { ppCurrent         = xmobarColor (green myColor) "" . wrap "[" "]",
      ppVisible         = xmobarColor (white myColor) "" . wrap "" "" . clickable,
      ppHidden          = xmobarColor (yellow myColor) "" . wrap "" "" . clickable,
      ppHiddenNoWindows = xmobarColor (white myColor) "" . clickable,
      ppSep             = " | ",
      ppTitle           = xmobarColor (white myColor) "" . shorten 60,
      ppLayout          = xmobarColor  (white myColor) "",
      ppOutput          = hPutStrLn h,
      --ppExtras          = [windowCount],
      ppOrder           = \(ws : l : t : ex) -> [ws, l, t]
    }

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--startuphook
myStartupHook :: X ()
myStartupHook = do
  --spawn "~/.config/xmonad/scripts/pipes.sh &"
  --spawn "~/.config/xmonad/scripts/volume-pipe.sh &"
  --spawn "~/.config/xmonad/scripts/backlight-pipe.sh &"
   spawn "~/.config/xmonad/scripts/systray.sh &"

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
-- keybidings and keychords
myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    [
      -- spawn applications
      ((modm, xK_t),                     spawn $ XMonad.terminal conf),
      ((modm .|. shiftMask, xK_t),       spawn myTerminalAlt),
      ((modm, xK_f),                     spawn myFilemanager),
      ((modm .|. shiftMask, xK_f),       spawn myFilemanagerAlt),
      ((modm, xK_b),                     spawn myBrowser),
      ((modm .|. shiftMask, xK_b),       spawn myBrowserAlt),
      ((modm, xK_m),                     spawn myMail),
      ((modm .|. shiftMask, xK_m),       spawn myMusicplayer),
      ((modm, xK_r),                     spawn myRssreader),
      ((modm, xK_e),                     spawn myIDE),

      -- prompts
      ((modm, xK_space),                 spawn "rofi -show run"),
      ((modm, xK_Tab),                   spawn "rofi -show window"),
      ((modm .|. shiftMask, xK_space),   spawn "rofi -show windowcd"),
      ((modm, xK_i),                     spawn "~/.config/xmonad/scripts/sysinfo.sh"),


      -- kill compile exit lock
      ((modm, xK_q),                     kill1),
      ((modm .|. shiftMask, xK_q),       kill),
      ((modm, xK_c),                     spawn "xmonad --recompile; xmonad --restart"),
      ((modm .|. shiftMask, xK_c),       io exitSuccess),

      -- layout change focus
      ((modm, xK_j),                     windows W.focusDown),
      ((modm, xK_k),                     windows W.focusUp),

      -- shift windows
      ((modm .|. shiftMask, xK_j),       windows W.swapDown),
      ((modm .|. shiftMask, xK_k),       windows W.swapUp),
      ((modm, xK_Return),                windows W.swapMaster),

      -- change layout
      ((modm, xK_n),                     sendMessage NextLayout),
      ((modm .|. shiftMask, xK_n),       setLayout $ XMonad.layoutHook conf),

      -- resize windows and float
      ((modm .|. controlMask, xK_j),     sendMessage MirrorShrink),
      ((modm .|. controlMask, xK_k),     sendMessage MirrorExpand),
      ((modm .|. controlMask, xK_h),     sendMessage Shrink),
      ((modm .|. controlMask, xK_l),     sendMessage Expand),
      ((modm .|. controlMask, xK_t),     withFocused $ windows . W.sink),

      -- copy window to all workspace
      ((modm, xK_0),                     windows copyToAll),
      ((modm .|. shiftMask, xK_0),       killAllOtherCopies),

      -- gaps and struts and fullscreen
      ((modm, xK_equal),                 sequence_ [incWindowSpacing 2, incScreenSpacing 2]),
      ((modm, xK_minus),                 sequence_ [decWindowSpacing 2, decScreenSpacing 2]),
      ((modm .|. controlMask, xK_equal), sequence_ [setWindowSpacing (Border 0 myWindowGap 0 myWindowGap), setScreenSpacing (Border 0 myWindowGap 0 myWindowGap)]),
      ((modm .|. controlMask, xK_f),     sequence_ [sendMessage ToggleStruts, toggleScreenSpacingEnabled, toggleWindowSpacingEnabled]),

      -- screenshots
      ((modm, xK_Print),                   spawn "~/scripts/sc -f"),
      ((modm .|. shiftMask, xK_Print),     spawn "~/scripts/sc -s"),
      ((modm .|. controlMask, xK_Print),   spawn "~/scripts/sc -cs"),
      ((mod1Mask, xK_Print),               spawn "~/scripts/sc -f -r"),
      ((mod1Mask .|. shiftMask, xK_Print), spawn "~/scripts/sc -s -r"),

      ((modm, xK_p),                   spawn "~/scripts/sc -f"),
      ((modm .|. shiftMask, xK_p),     spawn "~/scripts/sc -s"),
      ((modm .|. controlMask, xK_p),   spawn "~/scripts/sc -cs"),
      ((mod1Mask, xK_p),               spawn "~/scripts/sc -f -r"),
      ((mod1Mask .|. shiftMask, xK_p), spawn "~/scripts/sc -s -r"),

      --volume
      ((0, xF86XK_AudioMute),            spawn "amixer set Master 'toggle'"),
      ((0, xF86XK_AudioRaiseVolume),     spawn "amixer set Master 5%+"),
      ((0, xF86XK_AudioLowerVolume),     spawn "amixer set Master 5%-"),
      ((modm, xK_Up),     spawn "amixer set Master 5%+"),
      ((modm, xK_Down),     spawn "amixer set Master 5%-"),

      -- backlight
      ((0, xF86XK_MonBrightnessUp),      spawn "xbacklight -inc 5"),
      ((0, xF86XK_MonBrightnessDown),    spawn "xbacklight -dec 5"),
      ((modm, xK_Right),      spawn "xbacklight -inc 5"),
      ((modm, xK_Left),       spawn "xbacklight -dec 5")

    ]
      ++
      --change workspace
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9],
          (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
      ]
      ++
      --move windows to workspaces
      [ ((m .|. modm, k), windows $ f i)
        | (i, k) <- zip myWorkspaces [xK_1 ..],
          (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]
      ]

      -- dmenu submap
      ++ [ ( (modm, xK_s), submap . M.fromList $ [((0, k),  spawn e) | (k, e) <- myPromptList])]

      -- scratchpad submap
      ++ [ ( (modm, xK_d), submap . M.fromList $ [((0, k),  namedScratchpadAction myScratchPads e) | (k, e) <- myScratchPadList])]

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--mousebinding
myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster), -- mod-button1, Set the window to floating mode and move by dragging
      ((modm, button2), \w -> focus w >> windows W.shiftMaster), -- mod-button2, Raise the window to the top of the stack
      ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster) -- mod-button3, Set the window to floating mode and resize by dragging
    ]

--Workspaces
myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]
myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]
clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
--Main
main :: IO ()
main = do
  myXmobar <- spawnPipe "xmobar -x 0 ~/.config/xmonad/xmobar.config"
  xmonad $ docks $ ewmh def
        { terminal           = myTerminal,
          focusFollowsMouse  = myFocusFollowsMouse,
          clickJustFocuses   = myClickJustFocuses,
          borderWidth        = myBorderWidth,
          modMask            = myModMask,
          workspaces         = myWorkspaces,
          focusedBorderColor = myFocusedBorderColor,
          normalBorderColor  = myUnFocusedBorderColor,
          keys               = myKeys,
          layoutHook         = myLayout,
          manageHook         = myManageHook,
          handleEventHook    = myEventHook,
          logHook            = dynamicLogWithPP $ myXmobarPP myXmobar,
          startupHook        = myStartupHook
        }
