#!/bin/sh
# x.sh -- delegate script for dbus dependent parts of xinit.

. $MYLIB/fns.sh

# Some programs require dbus started, so this script here is to be run
# by ‘dbus-launch’ and it starts dbus-dependent programs before it
# hands the control to the window manager.

logx Start sbindkeys
(sleep 2; xbindkeys) &

logx Set up workspace
xclock -geometry 125x125-0-0 &
xterm +sb -fs 9 -geometry 35x7-3-130 -T xstatus -e\
      'watch --interval 60 --no-title --color status.sh' &

# Set the background.
feh --no-fehbg --bg-fill ~/pic/wlp/415656-2048x1365.jpg &

logx Start the window manager, xinit scripts completed
exec vtwm -f $MEINE/twmrc
