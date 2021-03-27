#!/bin/sh
# lockscr.sh --- lock screen in X session

bg="${GK_XBGIMG-$HOME/.xbg.png}"

# if image not found, display a gray screen
i3lock -p default -f -e -i "$bg" -c 333333 && sleep 1 || exit 2

