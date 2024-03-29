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

# Set up access to Xorg and i3wm.
config="$(/bin/pgrep -a Xorg | tr -s ' ' | cut -d ' ' -f 3,7)"
config=( $config )
DISPLAY="${config[0]}"
XAUTHORITY="${config[1]}"
export DISPLAY XAUTHORITY
I3SOCK=$(i3 --get-socketpath || :)
export I3SOCK

# HACK(2023-08-19): this is set in $MYSYSTEM/desktop-setup.bash,
# shouldn’t rely on this...
GK_XBGIMG="${GK_XBGIMG:-$HOME/.xbg.png}"

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
    /bin/sh $HOME/.screenlayout/sappho-docked-only-external.sh

    # Refresh the wallpaper
    feh --no-fehbg --bg-fill $GK_XBGIMG

    # Adjust i3wm setup
    if [ -n "$I3SOCK" ]; then
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

    # Refresh the wallpaper
    feh --no-fehbg --bg-fill $GK_XBGIMG

    # Adjust i3wm setup
    if [ -n "$I3SOCK" ]; then
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
