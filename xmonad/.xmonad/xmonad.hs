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
--import XMonad.Hooks.ICCCMFocus
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


import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

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

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "dmenu_run")

    ---- launch gmrun
    , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    ---- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_r     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- h/j/k/l keys added by withWindowNavigation
    ---- Move focus to the next window
    --, ((modm,               xK_j     ), windows W.focusDown)

    ---- Move focus to the previous window
    --, ((modm,               xK_k     ), windows W.focusUp  )

    ---- Move focus to the master
    --, ((modm,               xK_h     ), windows W.focusMaster )

    ---- Move focus to the not-master
    --, ((modm,               xK_l     ), windows W.focusDown )

    ---- Move focus to the master window
    --, ((modm,               xK_m     ), windows W.focusMaster  )

    ---- Swap the focused window and the master window
    --, ((modm,               xK_Return), windows W.swapMaster)

    ---- Swap the focused window with the next window
    --, ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    ---- Swap the focused window with the previous window
    --, ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    ---- Shrink the master area
    , ((modm .|. mod1Mask, xK_h     ), sendMessage Shrink)

    ---- Expand the master area
    , ((modm .|. mod1Mask, xK_l     ), sendMessage Expand)

--     , ((modm .|. shiftMask,               xK_w     ), withFocused (keysResizeWindow (0,10) (0.5,0.5)))
--     , ((modm .|. shiftMask,               xK_s     ), withFocused (keysResizeWindow (0,-10) (0.5,0.5)))
--     , ((modm .|. shiftMask,               xK_d     ), withFocused (keysResizeWindow (10,0) (0.5,0.5)))
--     , ((modm .|. shiftMask,               xK_a     ), withFocused (keysResizeWindow (-10,0) (0.5,0.5)))
--     , ((modm,               xK_w     ), withFocused (keysMoveWindow (0,-10)))
--     , ((modm,               xK_s     ), withFocused (keysMoveWindow (0,10)))
--     , ((modm,               xK_d     ), withFocused (keysMoveWindow (10,0)))
--     , ((modm,               xK_a     ), withFocused (keysMoveWindow (-10,0)))


    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)
    -- Float window
--    , ((modm,               xK_f     ), withFocused float)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    -- for xquartz
    --, ((mod1Mask .|. controlMask, xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Lock screen
    , ((mod1Mask .|. controlMask, xK_l ), spawn "/usr/bin/xscreensaver-command -lock")

    -- swap clip/primary
    , ((modm .|. shiftMask, xK_v     ), spawn "/home/pcl/swapbuf.sh")

    -- copy window
    , ((modm .|. controlMask, xK_t ), windows copyToAll) -- @@ Make focused window always visible
    , ((modm .|. shiftMask, xK_t ),  killAllOtherCopies) -- @@ Toggle window state back

    -- Skip song
    , ((modm,               xK_n     ), spawn "/usr/local/bin/spot next")

    -- 4 screens
    , ((modm,               xK_grave ), layoutSplitScreen 4 Grid)
    , ((modm .|. mod1Mask,  xK_grave ), layoutSplitScreen 2 $ Mirror $ TwoPane (3/100) (1/2))
    , ((modm .|. shiftMask, xK_grave ), rescreen)

    , ((modm ,              xK_equal ), layoutSplitScreen 5 $ ThreeColMid 1 (3/100) (1/2))
    , ((modm .|. mod1Mask,  xK_equal ), layoutSplitScreen 3 $ Mirror $ Tall 1 (3/100) (3/4))
    , ((modm .|. shiftMask,  xK_equal ), layoutSplitScreen 3 $ ThreeColMid 1 (3/100) (1/2))
   -- , ((modm .|. mod1Mask,  xK_equal ), layoutSplitScreen 5 $ Mirror $ ThreeColMid 1 (3/100) (1/2))
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- numpad on xquartz
    --[((m .|. modm, k), windows $ f i)
    --    | (i, k) <- zip (XMonad.workspaces conf) [xK_KP_1 .. xK_KP_9]
    --    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- Same thing, for numpad
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) numPadKeys
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]

   ++
    --
    -- mod-[asdfg], Switch to workspace N
    -- mod-shift-[asdfg], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_a, xK_s, xK_d, xK_f, xK_g]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    -- numpad on xquartz
    --[((m .|. modm, k), windows $ f i)
    --    | (i, k) <- zip (XMonad.workspaces conf) [xK_a, xK_s, xK_d, xK_f, xK_g]
    --    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++

    --
    -- Same thing, for numpad
    --
    --[((m .|. modm, k), windows $ f i)
    --  | (i, k) <- zip (XMonad.workspaces conf) numPadKeys
    --  , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++
--
-- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
-- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
--
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
       | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
       , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

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
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = fadeInactiveLogHook 0.90 -- <+> takeTopFocus -- return ()

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
    xmonad $ ewmh config

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
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
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }
