#!/bin/sh
# x.sh -- delegate script for dbus dependent parts of xinit.
# Some programs require dbus started, so this script here is to be run
# by ‘dbus-launch’ and it starts dbus-dependent programs before it
# hands the control to the window manager.

# Set up the desktop.
# -geometry args: wxh+x+y
#xclock -geometry 125x125-0-0&
#xterm -geometry 120x1-0+0 -T xstatus -e $MEINE/../scripts/sys-status.sh&

# Set the background.
feh --no-fehbg --bg-fill ~/Pictures/Wallpapers/415656-2048x1365.jpg

exec i3 #vtwm -f $MEINE/twmrc
