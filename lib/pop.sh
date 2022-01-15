#!/bin/sh
# pop.sh --- play alert sound

urgency=$DUNST_URGENCY
sounds_dir=/usr/share/sounds/freedesktop/stereo/

case $urgency in
    CRITICAL)	paplay $sounds_dir/message-new-instant.oga ;;
    NORMAL)	paplay $sounds_dir/dialog-information.oga ;;
    *)          paplay $sounds_dir/dialog-information.oga ;;
esac
