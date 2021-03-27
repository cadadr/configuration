#!/bin/sh
# lockscr.sh --- lock screen in X session

bg="${GK_XBGIMG-$HOME/.xbg.png}"

# if image not found, display a gray screen
exec i3lock -p default -f -n -e -i "x$bg" -c 333333

