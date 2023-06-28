#!/usr/bin/env bash
# tpx230.bash --- hardware settings for ThinkPad x230

# bash strict mode
set -euo pipefail
IFS=$'\n\t'


# Disable the annoying ThinkPad touchpad
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'Device Enabled' 0

# Natural scrolling with the nipple
# xinput set-prop 'TPPS/2 IBM TrackPoint' 'libinput Natural Scrolling Enabled' 1
