#!/usr/bin/env bash
# dock.bash --- thinkpad docking handler for sappho

# Fork for udev.
if [ "$2" != "forked" ]; then
    su -c "$(dirname $0)/$(basename $0) $1 forked" - cadadr &
    disown
    exit
fi

# bash strict mode
set -euo pipefail

config="$(/bin/pgrep -a Xorg | tr -s ' ' | cut -d ' ' -f 3,7)"
config=( $config )
DISPLAY="${config[0]}"
XAUTHORITY="${config[1]}"
export DISPLAY XAUTHORITY

on_docked(){
    notify-send -t 5000 docked "setting up..."

    # Wait for all devices to be detected.
    sleep 5

    # Keyboard
    setxkbmap gb
    setxkbmap -option  ctrl:nocaps

    # Logitech USB Trackball
    #
    # Set small button on the right as middle click, and disable history
    # navigation.
    xinput set-button-map "Logitech USB Trackball" \
        1 0 3 4 5 6 7 2 0
    # Make button 8 (left small) toggle scroll mode.
    xinput set-prop "Logitech USB Trackball" \
        "libinput Button Scrolling Button Lock Enabled" 1 || true

    # Monitors
    /bin/sh $HOME/.screenlayout/sappho-docked-only-external-vertical.sh

    # update background slideshow symlink
    rm -f $HOME/.gk-xbg-dir
    ln -s $HOME/Pictures/wlp/slideshow-vertical $HOME/.gk-xbg-dir

    # advance wallpaper for correctly cropped version.
    if [ -e $HOME/.setbg.bash.pid ]; then
        kill -USR1 "$(cat $HOME/.setbg.bash.pid)"
    fi

    notify-send -t 3000 docked "done!"
}

on_undocked(){
    notify-send -t 3000 undocked

    # Monitors
    /bin/sh $HOME/.screenlayout/sappho-undocked.sh

    rm -f $HOME/.gk-xbg-dir
    ln -s $HOME/Pictures/wlp/slideshow-horizontal $HOME/.gk-xbg-dir

    # advance wallpaper for correctly cropped version.
    if [ -e $HOME/.setbg.bash.pid ]; then
        kill -USR1 "$(cat $HOME/.setbg.bash.pid)"
    fi
}


case $1 in
    docked)	on_docked ;;
    undocked)	on_undocked ;;
    *)		exit 2 ;;
esac
