import XMonad

-- additional keybindings
import XMonad.Util.EZConfig
import XMonad.Util.Ungrab
import Graphics.X11.ExtraTypes.XF86 -- stuff like audio

-- Layout stuff
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Magnifier

-- Makes xmonad EWMH compliant
import XMonad.Hooks.EwmhDesktops

-- Hook stuff
import XMonad.Hooks.DynamicLog

-- Utilities
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers

-- Some configurations
myTerminal :: String
myTerminal = "alacritty" -- terminal emulator 

myModMask :: KeyMask
myModMask  = mod4Mask    -- Mod to super key

myBorderWidth :: Dimension
myBorderWidth = 1        -- Sets border width for windows

myNormColor :: String
myNormColor = "#282c34"  -- Non-focused windows border color

myFocusColor :: String
myFocusColor = "#ff6a45" -- Focus window border color
 
-- xmobar stuff
myBar :: String
myBar = "xmobar -x 1"

toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig{ modMask = m } = (m, xK_b)

myXmobarPP :: PP
myXmobarPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "[" "]" }

-- Layout
myLayout = tiled ||| Mirror tiled ||| Full -- ||| threeCol
  where
    --threeCol = magnifiercz' 1.3 $ ThreeColMid nmaster delta ratio
    tiled    = Tall nmaster delta ratio
    nmaster  = 1      -- Default number of windows in the master pane
    ratio    = 1/2    -- Default proportion of screen occupied by master pane
    delta    = 3/100  -- Percent of screen to increment by when resizing panes

myKeys :: [(String, X ())]
myKeys =
  [ ("M-S-z", spawn "xscreensaver-command -lock") -- Screenlock
  -- Applications
  , ("M-]"  , spawn "brave")            -- Brave
  , ("M-S-t", spawn "telegram-desktop") -- Telegram
  , ("M-C-p", spawn "spectacle -r -c")  -- Print rectangle and copy 
  -- Keyboard Layout 
  , ("M-S-k"  , spawn "setxkbmap -layout br -variant nodeadkeys -option caps:swapescape")
  , ("M-C-k", spawn "setxkbmap -layout br -option caps:swapescape")
  -- Audio 
  , ("M-S-o", spawn "pactl set-sink-volume @DEFAULT_SINK@ -5%") -- -5% volume
  , ("M-S-p", spawn "pactl set-sink-volume @DEFAULT_SINK@ +5%") -- +5% volume
  ]

-- Startup stuff
myStartupHook :: X ()
myStartupHook = do
  -- Load keyboard layout
  spawnOnce "setxkbmap -layout br -variant nodeadkeys -option caps:swapescape"
  -- Screen setup stuff
  spawnOnce "xrandr --output HDMI1 --auto"
  spawnOnce "xrandr --output eDP-1 --auto --right-of HDMI-1"
  -- Wallpaper, screen-saver and screen temperature color
  spawnOnce "feh --bg-fill --no-fehbg ~/Pictures/closing_hours.jpg"
  spawnOnce "xscreensaver -no-splash"
  spawnOnce "redshift -P -O 4500"
  -- Trayer stuff
  --spawnOnce "nm-applet"
  spawnOnce "trayer --edge top --align right --widthtype request --padding 6 --SetDockType true --SetPartialStrut true --expand true --monitor 0 --transparent true --alpha 0 --tint 0x282c34  --height 18 &"

-- Sum up of the configurations
myConfig = def
  { terminal           = myTerminal
  , modMask            = myModMask
  , startupHook        = myStartupHook
  , layoutHook         = myLayout
  , borderWidth        = myBorderWidth
  , normalBorderColor  = myNormColor
  , focusedBorderColor = myFocusColor
  }
  `additionalKeysP` myKeys

main :: IO ()
main = xmonad . ewmh =<< statusBar myBar myXmobarPP toggleStrutsKey myConfig
