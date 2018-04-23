-- gk-xmonad.hs --- Göktuğ's XMonad Configuration.

import XMonad
import XMonad.Config.Desktop (desktopConfig)
import XMonad.Util.EZConfig (additionalKeys)
import XMonad.Layout.Spacing (spacing)

baseConfig = desktopConfig

main = xmonad $ baseConfig {
    modMask = myModMask,
    -- First two workspaces are the most frequent ones.
    workspaces = ["home", "other"] ++ (map show $ takeWhile (<=9) [3..]),
    layoutHook = myLayoutHook
  } `additionalKeys` myKeys

myLayoutHook = spacing 2 $ layoutHook baseConfig

myModMask = mod4Mask
modShiftMask = myModMask .|. shiftMask

notify sub msg = spawn ("notify-send '" ++ sub ++ "' '" ++ msg ++ "'")

spawnAndNotify cmd sub msg = do spawn cmd
                                notify cmd msg

myKeys = [((myModMask, xK_q), (spawn "start-qutebrowser.sh")),
          -- Use the sct program to set screen redness.
          ((myModMask, xK_n),
           (spawnAndNotify "sct 4000" "Night mode" "Turned on.")),
          ((myModMask, xK_d),
           (spawnAndNotify "sct 12000" "Night mode" "Turned off.")),
          ((myModMask, xK_p), (spawn "dmenu-desktop.pl")),
          ((modShiftMask, xK_p), (spawn "dmenu_run")),
          ((myModMask, xK_s), (spawn "status.sh")),
          ((myModMask, xK_l), (spawn "i3lock -c dddeee"))]


-- Local Variables:
-- indent-tabs-mode: nil
-- compile-command: "make xmonad && xmonad --restart"
-- End:
