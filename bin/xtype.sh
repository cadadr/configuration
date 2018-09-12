#!/bin/sh
# xtype.sh --- choose a window and type command line arguments into it

if [ x"$*" = x ]; then
    echo usage: xtype.sh STR [STR..]
    exit 1
fi

xdotool type -window "$(xwininfo | grep 'Window\ id:' | cut -d ' ' -f4)" "$*"

