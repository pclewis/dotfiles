--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Data.Monoid
import System.Exit
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ICCCMFocus
import XMonad.Actions.CopyWindow
import XMonad.Layout.WindowNavigation
import XMonad.Actions.WindowNavigation
import XMonad.Hooks.FadeInactive
import XMonad.Layout.NoBorders
import XMonad.Actions.FloatKeys
import XMonad.Util.Replace
import XMonad.Layout.LayoutScreens
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Column
import XMonad.Actions.UpdatePointer
import XMonad.Actions.Submap
import XMonad.Actions.ShowText
import XMonad.Util.Run
import XMonad.Util.EZConfig
import System.IO (hClose, hFlush, Handle)
import Data.Maybe (fromMaybe, fromJust)


import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1","2","3","4","5","6","7","8","9"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#000000"
myFocusedBorderColor = "#0000ff"

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
                                , "purple"       -- key color
                                , "white"        -- cmd color
                                , "Input\\ Mono" -- font
                                , "18"           -- line height
                                ]

  return handle

toSubmap :: XConfig l -> String -> [(String, X ())] -> X ()
toSubmap c name m = do
  pipe <- keyMapDoc name
  submap $ mkKeymap c m
  io $ hClose pipe

-- Note: Formatting is important for script
focusKeymap = -- Focus
  [ ("f", focus "Vimperator") -- vimperator
  , ("e", focus "emacs") -- Emacs
  , ("w", focus "Weechat") -- Weechat
  , ("c", focus "Chromium") -- Chromium
  , ("m", windows W.focusMaster) -- Focus Master
  , ("/", spawn menu)
  ]
  where focus :: String -> X ()
        focus w = spawn ("wmctrl -a " ++ w)
        menu = "wmctrl -l | cut -d' ' -f 5- | sort | uniq -u | dmenu -i | xargs -IWIN wmctrl -F -a WIN"

musicKeymap = -- Music
  [ ("n", mpc "next") -- Next
  , ("N", mpc "prev") -- Prev
  , ("p", mpc "toggle") -- Toggle
  , ("r", mpc "random") -- Random
  , ("l", mpc "repeat") -- Repeat
  ]
  where mpc c = spawn ("mpc " ++ c)

masterKeymap = -- Master Window
  [ ("f",   windows W.focusMaster) -- Focus
  , ("s",   windows W.swapMaster) -- Swap
  , ("h",   sendMessage Shrink) -- Shrink
  , ("l",   sendMessage Expand) -- Expand
  , ("k",   incMaster) -- Inc
  , ("j",   decMaster) -- Dec
  ]
  where incMaster       = sendMessage (IncMasterN 1)
        decMaster       = sendMessage (IncMasterN (-1))

screenKeymap = -- Screen
  [ ("0", rescreen)
  , ("2", layoutSplitScreen 2 $ TwoPane (3/100) (1/2)) -- TwoPane
  , ("3", layoutSplitScreen 3 $ ThreeColMid 1 (3/100) (1/2)) -- ThreeColmid
  , ("4", layoutSplitScreen 4 Grid) -- Grid
  , ("5", layoutSplitScreen 5 $ ThreeColMid 1 (3/100) (1/2)) -- ThreeColmid
  ]

shotKeymap = -- Screen Shot
  [ ("c", setContext) -- Context
  , ("s", takeShot select) -- Select
  , ("w", takeShot currentWindow) -- Current Window
  , ("o", openDirectory)
  ]
  where setContext = spawn ("~/.xmonad/sshot-context.sh")
        takeShot a = spawn ("scrot " ++ a ++ " ~/screenshots/current-context/'%Y-%m-%dT%H%M%S_$wx$h.png'")
        openDirectory = spawn ("xdg-open ~/screenshots/current-context/")
        select        = "-s"
        currentWindow = "-u"

mainKeymap c = mkKeymap c $
    [ ("M-S-<Return>", spawn myTerminal) -- Terminal
    , ("M-p",          spawn "dmenu_run") -- Dmenu
    , ("M-S-c",        kill)
    , ("M-<Space>",    sendMessage NextLayout) -- Next Layout
    , ("M-<Tab>",      nextWindow) -- Next Window
    , ("M-S-<Tab>",    prevWindow) -- Prev Window
    , ("M-M1-h",       sendMessage Shrink) -- Shrink
    , ("M-M1-l",       sendMessage Expand) -- Expand
    , ("M-t",          withFocused $ windows . W.sink) -- Sink
    , ("M-q",          spawn "xmonad --recompile; xmonad --restart") -- Restart
    , ("M-S-q",        io $ exitWith ExitSuccess) -- Exit
    , ("C-M1-l",       spawn "xscreensaver-command -lock") -- Screen Lock
    , ("M-w",          toSubmap c "focusKeymap" focusKeymap) -- Focus
    , ("M-m",          toSubmap c "musicKeymap" musicKeymap) -- Music
    , ("M-a",          toSubmap c "masterKeymap" masterKeymap) -- Master
    , ("M-=",          toSubmap c "screenKeymap" screenKeymap) -- Screen
    , ("M-s",          toSubmap c "shotKeymap" shotKeymap) -- Screen Shot
    , ("M-S-/",        toSubmap c "mainKeymap" []) -- Main Menu
    ]
  where nextWindow      = windows W.focusDown
        prevWindow      = windows W.focusUp


    --  Reset the layouts on the current workspace to default
    -- , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)
    -- Resize viewed windows to the correct size
    -- , ((modm,               xK_r     ), refresh)
    -- swap clip/primary
    --, ((modm .|. shiftMask, xK_v     ), spawn "/home/pcl/swapbuf.sh")
    -- copy window
    --, ((modm .|. controlMask, xK_t ), windows copyToAll) -- @@ Make focused window always visible
    --, ((modm .|. shiftMask, xK_t ),  killAllOtherCopies) -- @@ Toggle window state back
    -- 4 screens
    --, ((modm,               xK_grave ), layoutSplitScreen 4 Grid)
    --, ((modm .|. mod1Mask,  xK_grave ), layoutSplitScreen 2 $ Mirror $ TwoPane (3/100) (1/2))
    --, ((modm .|. shiftMask, xK_grave ), rescreen)

    --, ((modm ,              xK_equal ), layoutSplitScreen 5 $ ThreeColMid 1 (3/100) (1/2))
    --, ((modm .|. mod1Mask,  xK_equal ), layoutSplitScreen 3 $ Mirror $ Tall 1 (3/100) (3/4))
    --, ((modm .|. shiftMask,  xK_equal ), layoutSplitScreen 3 $ ThreeColMid 1 (3/100) (1/2))
    -- , ((modm .|. mod1Mask,  xK_equal ), layoutSplitScreen 5 $ Mirror $ ThreeColMid 1 (3/100) (1/2))

-- Non-numeric num pad keys, sorted by number
numPadKeys = [ xK_KP_End,  xK_KP_Down,  xK_KP_Page_Down -- 1, 2, 3
             , xK_KP_Left, xK_KP_Begin, xK_KP_Right     -- 4, 5, 6
             , xK_KP_Home, xK_KP_Up,    xK_KP_Page_Up   -- 7, 8, 9
             , xK_KP_Insert]
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
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = noBorders $ column ||| tiled ||| Mirror tiled ||| Full
  where
     column = Column 1
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio

     -- The default number of windows in the master pane
     nmaster = 1

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
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Screenkey"      --> doFloat
    , resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "Xfce4-notifyd"  --> doIgnore
    ]

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = handleTimerEvent
--mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = (fadeInactiveLogHook 0.80)
            >> updatePointer (0.5, 0.5) (0, 0)
            >> takeTopFocus
 -- <+> takeTopFocus -- return ()

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
myStartupHook = ewmhDesktopsStartup >> setWMName "LG3D" -- return ()

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
    replace
    config <- withWindowNavigation (xK_k, xK_h, xK_j, xK_l) defaults
    xmonad $ ewmh config `additionalKeys` ([((m .|. myModMask, k), windows $ f i)
                                           | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]
                                           , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]])

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = mainKeymap,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
