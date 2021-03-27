#!/bin/sh
# lockscr.sh --- lock screen in X session

img="$HOME/.lockscr.png"

if [ ! -e "$img" ]; then
    s="$(xdpyinfo | grep dimensions: | sed -E 's,.*([0-9]{4}x[0-9]{3}).*,\1,')"
    convert "$XBGIMG" -gravity center -resize "$s" "$img"
fi

i3lock -p default -f -e -i "$img" && sleep 1

