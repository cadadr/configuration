#!/bin/sh
# pop.sh --- play alert sound

urgency=$5

case $urgency in
    CRITICAL)	paplay $HOME/cf/candy/radio.wav ;;
    NORMAL)	paplay $HOME/cf/candy/bleep.wav ;;
    *) ;;
esac
