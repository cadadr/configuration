#!/bin/sh
# xstartwm.sh --- start background programs and the window manager

# The main purpose of this file is to be run by dbus-launch(1),
# enabling both the WM and the background apps to be inherit the
# necessary environment variables.

set -e

. $MYLIB/xbackground.sh

exec $WM
