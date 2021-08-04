#!/usr/bin/env bash
# desktop-setup.bash --- initialisation for desktop session

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

# Hold the smaller left button to scroll in all directions.
xinput set-prop "Logitech USB Trackball" \
	'libinput Scroll Method Enabled' {0,0,1}

# Enable core dumps in case something goes wrong
ulimit -c unlimited

