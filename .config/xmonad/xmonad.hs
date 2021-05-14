--imports

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

-- layouts and layout modifiers
import XMonad.Layout.Tabbed
import XMonad.Layout.Spacing

-- layout modifier
import XMonad.Layout.Renamed
import XMonad.Layout.NoBorders
import XMonad.Layout.LayoutModifier(ModifiedLayout)
import XMonad.Layout.ShowWName
import XMonad.Layout.LimitWindows

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

--colors

black = "#282a36" -- black

red = "#ff5555" -- red

green = "#5af78e" -- green

yellow = "#f1fa8c" -- yellow

blue = "#57c7ff" -- blue

magenta = "#ff6ac1" -- magenta

cyan = "#8be9fd" -- cyan

white = "#f1f1f0" -- white

orange = "#ffb86c" -- orange

purple = "#bd9cf9" -- purple

-- user variables

myModMask :: KeyMask
myModMask = mod4Mask

myTerminal :: String
myTerminal = "st"

myTerminalAlt :: String
myTerminalAlt = "alacritty"

myFilemanager :: String
--myFilemanager = myTerminal ++ " -e lf"
myFilemanager = "emacsclient -c -a '' --eval '(dired nil)'"

myFilemanagerAlt :: String
myFilemanagerAlt = "pcmanfm"

myBrowser :: String
myBrowser = "vimb"

myBrowserAlt :: String
myBrowserAlt = "brave"

myMail :: String
--myMail = "geary"
myMail = "emacsclient -c -a '' --eval '(mu4e)'"

myMusicplayer :: String
myMusicplayer = myTerminal ++ " -e ncmpcpp"

myRssreader :: String
--myRssreader = myTerminal ++ " -e newsboat"
myRssreader = "emacsclient -c -a '' --eval '(elfeed)'"

myIDE :: String
--myIDE = myTerminal ++ " -e nvim"
myIDE = "emacsclient -c -a emacs"
--myIDE = "~/Downloads/emacs/src/emacs"

myFont :: String
myFont = "xft:Hack Nerd Font:regular:size=12:antialias=true:hinting=true"

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myClickJustFocuses :: Bool
myClickJustFocuses = False

myBorderWidth :: Dimension
myBorderWidth = 2

myFocusedBorderColor :: String
myFocusedBorderColor = magenta

-- keybidings and keychords

myKeys conf@XConfig {XMonad.modMask = modm} =
  M.fromList $
    [ -- spawn applications
      ((modm, xK_t), spawn $ XMonad.terminal conf),
      ((modm .|. shiftMask, xK_t), spawn myTerminalAlt),
      ((modm, xK_f), spawn myFilemanager),
      ((modm .|. shiftMask, xK_f), spawn myFilemanagerAlt),
      ((modm, xK_b), spawn myBrowser),
      ((modm .|. shiftMask, xK_b), spawn myBrowserAlt),
      ((modm, xK_m), spawn myMail),
      ((modm .|. shiftMask, xK_m), spawn myMusicplayer),
      ((modm, xK_r), spawn myRssreader),
      ((modm, xK_e), spawn myIDE),
      ((modm, xK_space), spawn "dmenu_run"),
      -- kill compile exit lock
      ((modm, xK_q), kill1),
      ((modm .|. shiftMask, xK_q), kill),
      ((modm, xK_c), spawn "xmonad --recompile; xmonad --restart"),
      ((modm .|. shiftMask, xK_c), io exitSuccess),
      ((modm .|. shiftMask, xK_x), spawn "~/scripts/lock"),
      -- layout change focus
      ((modm, xK_Tab), windows W.focusDown),
      ((modm .|. shiftMask, xK_Tab), windows W.focusUp),
      ((modm, xK_j), windows W.focusDown),
      ((modm, xK_k), windows W.focusUp),
      -- shift windows
      ((modm .|. shiftMask, xK_j), windows W.swapDown),
      ((modm .|. shiftMask, xK_k), windows W.swapUp),
      ((modm, xK_Return), windows W.swapMaster),
      -- change layout
      ((modm, xK_n), sendMessage NextLayout),
      ((modm .|. shiftMask, xK_n), setLayout $ XMonad.layoutHook conf),
      -- resize windows and float
      ((modm .|. controlMask, xK_h), sendMessage Shrink),
      ((modm .|. controlMask, xK_l), sendMessage Expand),
      ((modm .|. controlMask, xK_t), withFocused $ windows . W.sink),
      -- copy window to all workspace
      ((modm, xK_0), windows copyToAll),
      ((modm .|. shiftMask, xK_0), killAllOtherCopies),
      -- gaps and struts and fullscreen
      ((modm .|. controlMask, xK_f), sequence_ [sendMessage ToggleStruts, toggleScreenSpacingEnabled, toggleWindowSpacingEnabled]),
      ((modm, xK_equal), sequence_ [incWindowSpacing 2, incScreenSpacing 2]),
      ((modm, xK_minus), sequence_ [decWindowSpacing 2, decScreenSpacing 2]),
      -- screenshots
      ((modm, xK_Print), spawn "~/scripts/sc"),
      ((modm .|. shiftMask, xK_Print), spawn "~/scripts/sc -s"),
      ((modm .|. controlMask, xK_Print), spawn "~/scripts/sc -cs"),
      --volume
      ((0, xF86XK_AudioMute), spawn "~/scripts/bar/volume toggle"),
      ((0, xF86XK_AudioRaiseVolume), spawn "~/scripts/bar/volume up"),
      ((0, xF86XK_AudioLowerVolume), spawn "~/scripts/bar/volume down"),
      -- backlight
      ((0, xF86XK_MonBrightnessUp), spawn "~/scripts/bar/backlight up"),
      ((0, xF86XK_MonBrightnessDown), spawn "~/scripts/bar/backlight down")
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
      ++ [ ( (modm, xK_d),
             submap . M.fromList $
               [ ((0, xK_s), spawn "~/scripts/dpower"),
                 ((0, xK_p), spawn "~/scripts/bw"),
                 ((0, xK_m), spawn "~/scripts/dman"),
                 ((0, xK_k), spawn "~/scripts/dkill"),
                 ((0, xK_c), spawn "~/scripts/dcol"),
                 ((0, xK_w), spawn "~/scripts/dsearch"),
                 ((0, xK_e), spawn "~/scripts/dconfig")
               ]
           )
         ]
      ++ [ ( (modm, xK_s),
             submap . M.fromList $
               [ ((0, xK_a), spawn "~/scripts/dsearch archwiki"),
                 ((0, xK_p), spawn "~/scripts/dsearch aur"),
                 ((0, xK_d), spawn "~/scripts/dsearch duckduckgo"),
                 ((0, xK_g), spawn "~/scripts/dsearch google"),
                 ((0, xK_h), spawn "~/scripts/dsearch hoogle"),
                 ((0, xK_r), spawn "~/scripts/dsearch reddit"),
                 ((0, xK_s), spawn "~/scripts/dsearch startpage"),
                 ((0, xK_u), spawn "~/scripts/dsearch urbandictionary"),
                 ((0, xK_y), spawn "~/scripts/dsearch youtube")
               ]
           )
         ]
      ++
      -- scratchpad submap
      [ ( (modm, xK_p),
          submap . M.fromList $
            [ ((0, xK_h), namedScratchpadAction myScratchPads "htop"),
              ((0, xK_m), namedScratchpadAction myScratchPads "ncmpcpp")
            ]
        )
      ]

--mousebinding

myMouseBindings XConfig {XMonad.modMask = modm} =
  M.fromList
    [ ((modm, button1), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster), -- mod-button1, Set the window to floating mode and move by dragging
      ((modm, button2), \w -> focus w >> windows W.shiftMaster), -- mod-button2, Raise the window to the top of the stack
      ((modm, button3), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster) -- mod-button3, Set the window to floating mode and resize by dragging
    ]

-- clickable works

myWorkspaces = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

myWorkspaceIndices = M.fromList $ zip myWorkspaces [1 ..]

clickable ws = "<action=xdotool key super+" ++ show i ++ ">" ++ ws ++ "</action>"
  where
    i = fromJust $ M.lookup ws myWorkspaceIndices

-- Scratchpads

myScratchPads =
  [ NS "htop" spawnHtop findHtop manageHtop,
    NS "ncmpcpp" spawncmpcpp findncmpcpp managncmpcpp
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
    spawncmpcpp = myTerminal ++ " -e ncmpcpp"
    findncmpcpp = title =? "ncmpcpp"
    managncmpcpp = customFloating $ W.RationalRect l t w h
      where
        h = 0.90
        w = 0.95
        t = (1 - h) / 2
        l = (1 - w) / 2

--Layouts

mySpacing :: Integer -> l a -> ModifiedLayout Spacing l a
mySpacing i = spacingRaw False (Border 0 i 0 i) True (Border i 0 i 0) True

tall =
  renamed [Replace "Tall"] $
    mySpacing 6 $
      limitWindows 7 $
        Tall 1 (3 / 100) (1 / 2)

wide =
  renamed [Replace "Wide"] $
    mySpacing 6 $
      limitWindows 5 $
        Mirror (Tall 1 (3 / 100) (1 / 2))

tabs =
  renamed [Replace "Tab"] $
    mySpacing 0 $
      limitWindows 15 $
        tabbed shrinkText myTabTheme

full =
  renamed [Replace "Full"] $
    mySpacing 6 $
      limitWindows
        15
        Full

myLayout =
  avoidStruts $
    smartBorders myDefaultLayout
  where
    myDefaultLayout =
      tall
        ||| wide
        ||| tabs
        ||| full

myTabTheme =
  def
    { fontName = myFont,
      activeColor = black,
      inactiveColor = black,
      activeBorderColor = purple,
      inactiveBorderColor = black,
      activeBorderWidth = 0,
      inactiveBorderWidth = 0,
      activeTextColor = magenta,
      inactiveTextColor = white
    }

myShowWNameTheme :: SWNConfig
myShowWNameTheme =
  def
    { swn_font = "xft:Hack Nerd Font:bold:size=60",
      swn_fade = 1.0,
      swn_bgcolor = "#282a36",
      swn_color = "#f1f1f0"
    }

--managehook

myManageHook =
  composeAll
    [ manageDocks,
      className =? "Steam" --> doFloat,
      className =? "Pavucontrol" --> doFloat,
      className =? "vlc" --> doFloat,
      title =? "Picture in Picture" --> doFloat,
      className =? "Freetube" --> doFloat,
      className =? "VirtualBox Manager" --> doFloat,
      className =? "Steam" --> doShift (myWorkspaces !! 2),
      className =? "csgo_linux64" --> doShift (myWorkspaces !! 3)
    ]
    <+> namedScratchpadManageHook myScratchPads

-- eventhook

myEventHook = mempty

-- dynamicloghook

--windowCount :: X (Maybe String)
--windowCount = gets $ Just . show . length . W.integrate' . W.stack . W.workspace . W.current . windowset

myXmobarPP h =
  xmobarPP
    { ppCurrent = xmobarColor green "" . wrap "[" "]",
      ppVisible = xmobarColor purple "" . wrap "" "" . clickable,
      ppHidden = xmobarColor yellow "" . wrap "" "" . clickable,
      ppHiddenNoWindows = xmobarColor purple "" . clickable,
      ppSep = "<fc=white> | </fc>",
      ppTitle = xmobarColor cyan "" . shorten 60,
      ppLayout = xmobarColor magenta "",
      ppOutput = hPutStrLn h,
      --, ppExtras  = [windowCount]
      ppOrder = \(ws : l : t : ex) -> [ws, l] ++ ex ++ [t]
    }

--myXmobarPP h =
--  xmobarPP
--    { ppCurrent = xmobarColor white "" . wrap "[" "]",
--      ppVisible = xmobarColor white "" . wrap "" "" . clickable,
--      ppHidden = xmobarColor yellow "" . wrap "" "" . clickable,
--      ppHiddenNoWindows = xmobarColor white "" . clickable,
--      ppSep = "<fc=white> | </fc>",
--      ppTitle = xmobarColor white "" . shorten 60,
--      ppLayout = xmobarColor white "",
--      ppOutput = hPutStrLn h,
--      --, ppExtras  = [windowCount]
--      ppOrder = \(wsltex) -> [ws, l] ++ ex ++ [t]
--    }

-- startuphook

myStartupHook = do
  spawn "~/.config/xmobar/pipes &"
  spawn "~/scripts/bar/volume &"
  spawn "~/scripts/bar/backlight &"
  spawn "sleep 1 && killall trayer; trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --height 20 --distance 2 --distancefrom right --transparent true --alpha 0 --tint 0x282a36 --widthtype request --monitor 0 --margin 2 &"
  spawn "killall connman-gtk; connman-gtk"

--Main

main :: IO ()
main = do
  myXmobar <- spawnPipe "xmobar -x 0 /home/sam/.config/xmobar/xmobar.config"
  xmonad $
    docks $
        def
          { terminal = myTerminal,
            focusFollowsMouse = myFocusFollowsMouse,
            clickJustFocuses = myClickJustFocuses,
            borderWidth = myBorderWidth,
            modMask = myModMask,
            workspaces = myWorkspaces,
            focusedBorderColor = myFocusedBorderColor,
            keys = myKeys,
            layoutHook = showWName' myShowWNameTheme myLayout,
            manageHook = myManageHook,
            handleEventHook = myEventHook,
            logHook = dynamicLogWithPP $ myXmobarPP myXmobar,
            startupHook = myStartupHook
          }
