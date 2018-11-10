#!/bin/sh
# space-as-ctrl.sh --- use the space key as another Ctrl

xmodmap -e 'keycode 65 = 0x1234'
xmodmap -e 'add control = 0x1234'
xmodmap -e 'keycode any = space'
xcape -e '0x1234=space'
