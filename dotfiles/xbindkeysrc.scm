;;; xbindkeysrc.scm --- X.org keybindings.

;; These keybindings are independent from the window manager.

;;; Sound:
(xbindkey
 '("XF86AudioRaiseVolume")
 "amixer set Master 2dB+ unmute &&\
  notify-send -t 1000 $(amixer get Master | grep '^  Mono' | cut -d ' ' -f6)")
(xbindkey
 '("XF86AudioLowerVolume")
 "amixer set Master 2dB- unmute &&\
  notify-send -t 1000 $(amixer get Master | grep '^  Mono' | cut -d ' ' -f6)")
(xbindkey
 '("XF86AudioMute")
 "amixer set Master toggle &&\
  notify-send -t 1000 $(amixer get Master | grep '^  Mono' | cut -d ' ' -f6,8)")

;;; Brightness:
(xbindkey '("XF86MonBrightnessDown") "xbacklight -dec 10%")
(xbindkey '("XF86MonBrightnessUp") "xbacklight -inc 10%")
