#!/usr/bin/env bash
# desktop-setup.bash --- initialisation for desktop session

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

# Swap mouse back/forward buttons around.
xinput set-button-map "USB Optical Mouse" 1 2 3 4 5 6 7 9 8
