#!/bin/sh
# gk-kde-settings.sh --- KDE settings hard to do from the config interface

# see: xinput list-props 'ETPS/2 Elantech Touchpad' | grep 'Click Method'
xinput --set-prop 'ETPS/2 Elantech Touchpad' 296 0 1
