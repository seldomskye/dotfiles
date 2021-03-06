import XMonad
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CopyWindow
import qualified Data.Map as M
import qualified XMonad.StackSet as W
import System.Exit
import XMonad.Hooks.SetWMName
import XMonad.Hooks.ManageHelpers
import XMonad.Layout.Spacing
import XMonad.Config.Xfce
main :: IO ()
main = do
    xmonad $ myConfig
      
myManageHook = composeAll
               [ isFullscreen --> doFullFloat
               , className =? "feh" --> doFloat
               , className =? "P" --> doFloat
               ]
myConfig = ewmh $ def {focusFollowsMouse = False
      , manageHook = myManageHook <+> manageHook def
      , layoutHook = smartSpacing 5 $ layoutHook def
      , modMask = myModMask
      , keys = myKeys
      , startupHook = do
          setWMName "LG3D" -- to let swing applications work because it's hard coded to check WM name.
          spawn "compton"
      , terminal = "urxvt"
      }

myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

myModMask :: KeyMask
myModMask = mod4Mask

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@(XConfig {XMonad.modMask = myModMask}) = M.fromList $

    -- launch a terminal
    [ ((myModMask .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((myModMask,               xK_p     ), spawn "gmrun")

    -- launch emacs
    , ((myModMask, xK_v), spawn "~/bin/ec")


    -- launch firefox
    , ((myModMask, xK_z), spawn "firefox")
    -- launch gmrun
    , ((myModMask .|. shiftMask, xK_p     ), spawn "gmrun")

    , ((myModMask .|. shiftMask, xK_d     ), spawn "scrot")
      
    -- close focused window
    , ((myModMask .|. shiftMask, xK_c     ), kill1)


     -- Rotate through the available layout algorithms
    , ((myModMask,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((myModMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((myModMask,               xK_k     ), refresh)

    -- Move focus to the next window
    , ((myModMask,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((myModMask,               xK_e     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((myModMask,               xK_i     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((myModMask,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((myModMask,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((myModMask .|. shiftMask, xK_e     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((myModMask .|. shiftMask, xK_i     ), windows W.swapUp    )

    -- Shrink the master area
    , ((myModMask,               xK_n     ), sendMessage Shrink)

    -- Expand the master area
    , ((myModMask,               xK_o     ), sendMessage Expand)

    -- Push window back into tiling
    , ((myModMask,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((myModMask              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((myModMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((myModMask              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((myModMask .|. shiftMask, xK_y     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((myModMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
        ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. myModMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_q, xK_w, xK_f] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
    ++
    [((m .|. myModMask, k), windows $ f i)
     | (i, k) <- zip (workspaces conf) [xK_1 ..]
     , (f, m) <- [(W.view, 0), (W.shift, shiftMask), (copy, shiftMask .|. controlMask)]]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. myModMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
