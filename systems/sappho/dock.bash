#!/usr/bin/env bash
# dock.bash --- thinkpad docking handler for sappho

# Fork for udev.
if [ "$2" != "forked" ]; then
    $(dirname $0)/$(basename $0) $1 forked &
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

    # Trackball [if present]
    # Hold the smaller left button to scroll in all directions.
    xinput set-prop "Logitech USB Trackball" 'libinput Scroll Method Enabled' {0,0,1} || true
    # Instead of holding, the button 8 toggles scroll mode.
    xinput set-prop "Logitech USB Trackball" "libinput Button Scrolling Button Lock Enabled" 1 || true

    # Monitors
    /bin/sh /home/cadadr/.screenlayout/sappho-docked-only-external.sh

    notify-send -t 3000 docked "done!"
}

on_undocked(){
    notify-send -t 3000 undocked

    # Monitors
    /bin/sh /home/cadadr/.screenlayout/sappho-undocked.sh
}


case $1 in
    docked)	on_docked ;;
    undocked)	on_undocked ;;
    *)		exit 2 ;;
esac
