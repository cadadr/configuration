#!/usr/bin/env bash
# logitech_trackman.bash --- logitech trackman USB trackball settings

# bash strict mode
set -euo pipefail
IFS=$'\n\t'


# Set small button on the right as middle click, and disable history
# navigation.
xinput set-button-map "Logitech USB Trackball" \
       1 0 3 4 5 6 7 0 2

# Enable scrolling. By default, the left small button needs to be
# pressed and held to use the track ball to scroll. But that’s an
# ergonomic nightmare so ...
xinput set-prop "Logitech USB Trackball" 'libinput Scroll Method Enabled' \
       {0,0,1}

# ... make button 8 (left small) toggle scroll mode.
xinput set-prop "Logitech USB Trackball" \
       "libinput Button Scrolling Button Lock Enabled" 1
