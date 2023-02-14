#!/usr/bin/env bash
# reorient-monitor.bash --- change the monitor orientation

. $MYLIB/fns2.sh

script_name="$(basename $0)"
script_usage=""

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

# Run last command in a pipeline in current shell (used for setting
# variable below).
shopt -s lastpipe

current_rotation="$(xrandr -q | grep 'connected primary' | awk '{print $5}')"

dmenu -p "Change monitor rotation to" <<END | \
    awk '{print $1}' | rotation="$(cat)"
normal   (horizontal)
right    (vertical, right side up / preferred for vertical)
inverted (horizontal, flipped vertically)
left     (vertical, left side up)
END

if [ "$rotation" = "$current_rotation" ]; then
    exit_with_error_message 2 "rotation is already $rotation"
fi

case $rotation in
    normal|inverted)
        direction=horizontal
        sh $HOME/.screenlayout/sappho-docked-only-external.sh
        ;;
    right|left)
        direction=vertical
        ROTATION="$rotation" sh $HOME/.screenlayout/sappho-docked-only-external-vertical.sh
        ;;
esac

# update background slideshow symlink
rm -f $HOME/.gk-xbg-dir
ln -s $HOME/Pictures/wlp/slideshow-$direction $HOME/.gk-xbg-dir

# advance wallpaper for correctly cropped version.
if [ -e $HOME/.setbg.bash.pid ]; then
    kill -USR1 "$(cat $HOME/.setbg.bash.pid)"
fi
