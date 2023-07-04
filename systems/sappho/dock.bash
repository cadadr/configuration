#!/usr/bin/env bash
# dock.bash --- thinkpad docking handler for sappho

# Fork for udev. The UID check guards for when script is run as the user.
if [ "$UID" = "0" ] && [ "$2" != "forked" ]; then
    su -c "$(dirname $0)/$(basename $0) $1 forked" - cadadr &
    disown
    exit
fi

. $HOME/.profile

# bash strict mode
set -euo pipefail

config="$(/bin/pgrep -a Xorg | tr -s ' ' | cut -d ' ' -f 3,7)"
config=( $config )
DISPLAY="${config[0]}"
XAUTHORITY="${config[1]}"
export DISPLAY XAUTHORITY
I3SOCK=$(i3 --get-socketpath || :)
export I3SOCK

# Args: inner outer horizontal vertical
set_gaps(){
    i3-msg -t command gaps inner current set $1
    i3-msg -t command gaps outer current set $2
    i3-msg -t command gaps horizontal current set $3
    i3-msg -t command gaps vertical current set $4
}

on_docked(){
    notify-send -u low -t 5000 docked "setting up..."

    # Wait for all devices to be detected.
    sleep 5

    # Make sure keyboard settings apply to any external keyboard too.
    bash $MYLIB/hw/keyboard.bash

    # If the USB trackball is attached, configure it.
    xinput list | grep "Logitech USB Trackball" >/dev/null 2>&1 \
        && bash $MYLIB/hw/logitech_trackman.bash

    # Monitor setup
    /bin/sh $HOME/.screenlayout/sappho-docked-only-external-vertical.sh

    # Update background slideshow symlink.
    rm -f $HOME/.gk-xbg-dir
    ln -s $HOME/Pictures/wlp/slideshow-vertical $HOME/.gk-xbg-dir

    # Advance wallpaper for correctly cropped version.
    if [ -e $HOME/.setbg.bash.pid ]; then
        kill -USR1 "$(cat $HOME/.setbg.bash.pid)"
    fi

    # Adjust i3wm setup
    if [ -n "$I3SOCK" ]; then
        # Larger gaps
        set_gaps 15 5 10 10

        # Show bottom info dock
        i3-msg -t command bar mode dock info

        # Swap layout for split virtual desktops
        # i3-appropriate-layout
    fi

    notify-send -u low -t 3000 docked "done!"
}

on_undocked(){
    notify-send -u low -t 3000 undocked "setting up..."

    # Monitor setup.
    /bin/sh $HOME/.screenlayout/sappho-undocked.sh

    rm -f $HOME/.gk-xbg-dir
    ln -s $HOME/Pictures/wlp/slideshow-horizontal $HOME/.gk-xbg-dir

    # Advance wallpaper for correctly cropped version.
    if [ -e $HOME/.setbg.bash.pid ]; then
        kill -USR1 "$(cat $HOME/.setbg.bash.pid)"
    fi

    # Adjust i3wm setup
    if [ -n "$I3SOCK" ]; then
        # Smaller gaps, should reflect the defaults in ~/.config/i3/config
        set_gaps 10 0 0 0

        # Hide bottom info dock. With the ‘hide’ mode, it reveals
        # while the $mod key is held down. The ‘invisible’ mode keeps
        # it invisible at all times.
        i3-msg -t command bar mode hide info

        # Swap layout for split virtual desktops
        # i3-appropriate-layout
    fi

    notify-send -u low -t 3000 undocked "done!"
}


case $1 in
    docked)	on_docked ;;
    undocked)	on_undocked ;;
    *)		exit 2 ;;
esac
