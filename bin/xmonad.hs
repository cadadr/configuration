-- gk-xmonad.hs --- Göktuğ's XMonad Configuration.

import XMonad (xmonad, modMask, mod4Mask, workspaces, spawn,
               xK_d, xK_n, xK_q)
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Util.EZConfig (additionalKeys)

main = xmonad $ desktopConfig {
    modMask = myModMask,
    -- First two workspaces are the most frequent ones.
    workspaces = ["home", "other"] ++ (map show $ takeWhile (<=9) [3..])
  } `additionalKeys` keys

myModMask = mod4Mask

keys = [((myModMask, xK_q), (spawn "start-qutebrowser.sh")),
        -- Use the sct program to set screen redness.
        ((myModMask, xK_n), (spawn "sct 4000")),
        ((myModMask, xK_d), (spawn "sct 12000"))]

