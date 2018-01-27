#!/bin/sh
# x.sh -- delegate script for dbus dependent parts of xinit.
# Some programs require dbus started, so this script here is to be run
# by ‘dbus-launch’ and it starts dbus-dependent programs before it
# hands the control to the window manager.

exec i3 #vtwm -f $MEINE/twmrc
