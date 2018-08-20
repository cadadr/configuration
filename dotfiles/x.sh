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
xterm +sb -geometry 120x2-0+0 -T xstatus -e i3status &

# Set the background.
feh --no-fehbg --bg-fill ~/Pictures/Wallpapers/415656-2048x1365.jpg

logx Start the window manager, xinit scripts completed
exec vtwm -f $MEINE/twmrc
