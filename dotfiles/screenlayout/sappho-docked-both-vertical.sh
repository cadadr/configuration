#!/bin/sh
xrandr --output LVDS1 --mode 1366x768 --pos 1080x1152 --rotate normal --output DP1 --off --output DP2 --off --output DP3 --off --output HDMI1 --off --output HDMI2 --primary --mode 1920x1080 --pos 0x0 --rotate ${ROTATION-right} --output HDMI3 --off --output VGA1 --off --output VIRTUAL1 --off
