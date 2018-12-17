#!/bin/sh
# pop.sh --- play alert sound

urgency=$5

case $urgency in
    CRITICAL) paplay $HOME/cf/candy/do-daeng.wav ;;
    NORMAL) paplay $HOME/cf/candy/cowbell.wav ;;
    *) ;;
esac
