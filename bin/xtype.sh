#!/bin/sh
# xtype.sh --- choose a window and type command line arguments or X clipboard contents into it

go () {
    xdotool type -delay 200ms \
	-window "$(xwininfo | grep 'Window\ id:' | cut -d ' ' -f4)" "$*"
}

if [ x"$*" = x ]; then
    go "$(xclip -o)"
else
    go "$*"
fi
