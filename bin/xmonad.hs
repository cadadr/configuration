-- -*- compile-command: "make xmonad && xmonad --restart" -*-
-- gk-xmonad.hs --- Göktuğ's XMonad Configuration.

import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Util.EZConfig (additionalKeys)

main = xmonad $ desktopConfig {
    modMask = myModMask,
    -- First two workspaces are the most frequent ones.
    workspaces = ["home", "other"] ++ (map show $ takeWhile (<=9) [3..])
  } `additionalKeys` myKeys

myModMask = mod4Mask
modShiftMask = myModMask .|. shiftMask

myKeys = [((myModMask, xK_q), (spawn "start-qutebrowser.sh")),
          -- Use the sct program to set screen redness.
          ((myModMask, xK_n), (spawn "sct 4000")),
          ((myModMask, xK_d), (spawn "sct 12000")),
          ((myModMask, xK_p), (spawn "dmenu-desktop.pl")),
          ((modShiftMask, xK_p), (spawn "dmenu_run")),
          ((myModMask, xK_s), (spawn "status.sh")),
          ((myModMask, xK_l), (spawn "i3lock -c dddeee"))]

