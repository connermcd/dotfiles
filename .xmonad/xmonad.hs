-------------------------------------------------------------------------------
-- xmonad.hs for xmonad-darcs
-------------------------------------------------------------------------------
-- Compiler flags --
{-# LANGUAGE NoMonomorphismRestriction #-}

-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import XMonad.Util.Run (safeSpawn)
import Graphics.X11.ExtraTypes.XF86

-- actions
import XMonad.Actions.GridSelect
import XMonad.Actions.CycleWS

-- hooks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed

-------------------------------------------------------------------------------
-- Main --
main :: IO ()
main = xmonad =<< statusBar cmd pp kb conf
  where
    uhook = withUrgencyHookC NoUrgencyHook urgentConfig
    cmd = ""
    --cmd = "bash -c \"tee >(xmobar -x0) | xmobar -x1 | xmobar -x2\""
    pp = customPP
    kb = toggleStrutsKey
    conf = uhook myConfig

-------------------------------------------------------------------------------
-- Configs --
myConfig = defaultConfig { workspaces = workspaces'
                         , modMask = modMask'
                         , borderWidth = borderWidth'
                         , normalBorderColor = normalBorderColor'
                         , focusedBorderColor = focusedBorderColor'
                         , terminal = terminal'
                         , keys = keys'
                         , mouseBindings = mouseBindings'
                         , layoutHook = layoutHook'
                         , manageHook = manageHook'
                         , handleEventHook = fullscreenEventHook
                         }

-------------------------------------------------------------------------------
-- Window Management --
manageHook' = composeAll [ isFullscreen             --> doFullFloat
                         , className =? "MPlayer"   --> doFloat
                         , className =? "Gimp"      --> doFloat
                         , className =? "Vlc"       --> doFloat
                         , className =? "feh"       --> doFloat
                         , insertPosition Below Newer
                         , transience'
                         ]


-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP = defaultPP { ppCurrent = xmobarColor "#B8860B" "" . wrap "<" ">"
                     , ppHidden = xmobarColor "#FAFFFF" ""
                     , ppHiddenNoWindows = xmobarColor "#FAFFFF" ""
                     , ppUrgent = xmobarColor "#FFFFAF" "" . wrap "[" "]"
                     , ppLayout = xmobarColor "#aaaaaa" ""
                     , ppTitle =  xmobarColor "#aaaaaa" "" . shorten 80
                     , ppSep = xmobarColor "#aaaaaa" "" " | "
                     }
-- GridSelect
myGSConfig = defaultGSConfig { gs_cellwidth = 160 }

-- urgent notification
urgentConfig = UrgencyConfig { suppressWhen = Focused, remindWhen = Dont }

-- borders
borderWidth' = 1
normalBorderColor'  = "#333333"
focusedBorderColor' = "#AFAF87"

-- tabs
tabTheme1 = defaultTheme { decoHeight = 16
                         , activeColor = "#a6c292"
                         , activeBorderColor = "#a6c292"
                         , activeTextColor = "#000000"
                         , inactiveBorderColor = "#000000"
                         }

-- workspaces
workspaces' = ["1", "2", "3", "4", "5", "6", "7", "8", "9"]

-- layouts
-- layoutHook' = tile ||| mtile ||| tab ||| full
layoutHook' = tile ||| full
  where
    rt = ResizableTall 1 (2/100) (1/2) []
    tile = renamed [Replace "[]="] $ smartBorders rt
    -- mtile = renamed [Replace "M[]="] $ smartBorders $ Mirror rt
    -- tab = renamed [Replace "T"] $ noBorders $ tabbed shrinkText tabTheme1
    full = renamed [Replace "[]"] $ noBorders Full

-------------------------------------------------------------------------------
-- Terminal --
terminal' :: String
terminal' = "urxvt"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' = mod4Mask

-- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask .|. shiftMask, xK_b)

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), safeSpawn terminal' ["-e", "bash", "-c", "tmux attach-session -d || tmux new-session -s main"])
    , ((modMask,               xK_space ), safeSpawn "dmenu_run" ["-b", "-nb", "black"])
    , ((modMask .|. shiftMask, xK_c     ), kill)

    -- multimedia
    , ((0, xF86XK_AudioRaiseVolume      ), safeSpawn "amixer" ["-q", "set", "Master", "1+"])
    , ((0, xF86XK_AudioLowerVolume      ), safeSpawn "amixer" ["-q", "set", "Master", "1-"])
    , ((0, xF86XK_AudioMute             ), safeSpawn "amixer" ["-q", "set", "Master", "toggle"])

    -- , ((modMask .|. shiftMask, xK_z     ), safeSpawn "i3lock" ["-c", "000000", "-n"])
    -- grid
    , ((modMask,               xK_g     ), goToSelected myGSConfig)

    -- layouts
    , ((modMask,               xK_f     ), sendMessage NextLayout)
    -- , ((modMask .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- floating layer stuff
    , ((modMask .|. shiftMask, xK_f     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_r     ), refresh)

    -- focus
    , ((modMask,               xK_Tab   ), windows W.focusDown)
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- workspaces
    , ((modMask,               xK_d     ), toggleWS)
    , ((modMask,               xK_p     ), prevWS)
    , ((modMask,               xK_n     ), nextWS)
    , ((modMask .|. shiftMask, xK_p     ), shiftToPrev >> prevWS)
    , ((modMask .|. shiftMask, xK_n     ), shiftToNext >> nextWS)

    -- screens
    , ((modMask,               xK_o     ), nextScreen)
    , ((modMask .|. shiftMask, xK_o     ), shiftNextScreen >> nextScreen)

    -- swapping
    , ((modMask .|. shiftMask, xK_Return), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_period), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- quit, or restart
    , ((modMask .|. shiftMask, xK_q     ), io exitSuccess)
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    -- ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    -- [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------
-- Mouse Bindings

mouseBindings' :: XConfig Layout -> M.Map (ButtonMask, Button) (Window -> X ())
mouseBindings' (XConfig {XMonad.modMask = modMask}) = M.fromList
  [ ((modMask, 1 :: Button), \w -> focus w >> mouseMoveWindow w >> windows W.shiftMaster)
  , ((modMask, 3 :: Button), \w -> focus w >> mouseResizeWindow w >> windows W.shiftMaster)
  , ((0      , 6 :: Button), \w -> focus w >> prevWS)
  , ((0      , 7 :: Button), \w -> focus w >> nextWS)
  , ((modMask, 6 :: Button), \w -> focus w >> shiftToPrev >> prevWS)
  , ((modMask, 7 :: Button), \w -> focus w >> shiftToNext >> nextWS)
  ]

-------------------------------------------------------------------------------
