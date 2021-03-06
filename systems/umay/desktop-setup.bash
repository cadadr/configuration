#!/usr/bin/env bash
# desktop-setup.bash --- initialisation for desktop session

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

# Swap mouse back/forward buttons around.
xinput set-button-map "USB Optical Mouse" 1 2 3 4 5 6 7 9 8

# Hold middle button to scroll in all directions.
xinput set-prop "USB Optical Mouse" \
	'libinput Scroll Method Enabled' {0,0,1}

xinput set-prop "Logitech USB Trackball" \
	'libinput Scroll Method Enabled' {0,0,1}

bash $MYLIB/launch-session-servers.bash
