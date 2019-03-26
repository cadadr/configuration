# xcommon.sh --- common Xorg setup

### Environment:
. $MYLIB/fns.sh

### Code:
# Inform systemd
systemctl --user import-environment DISPLAY

### X resources:
xrdb -merge $MEINE/Xdefaults

### Input:
#### Keyboard:
setxkbmap -option "ctrl:nocaps"

##### Mouse & Touchpad:
###### Multitouch clicks:
# Differentiate mouse buttons based on finger count instead of areas
# of the touchpad.  One finger is primary click (left button), two
# fingers are secondary click (right button), and three fingers are
# tertiary click (middle button).  Also, disable tapping.
propidRe='s/.*\(([0-9]+)\).*/\1/'

touchpad="$(xinput --list --name-only | grep -i touchpad)"

tappingToggle=$(xinput list-props "$touchpad" |\
			grep -i 'tapping enabled (' |\
			sed -E "$propidRe")

clickMethod=$(xinput list-props "$touchpad" |\
			grep -i 'click method enabled (' |\
			sed -E "$propidRe")

naturalScrolling=$(xinput list-props "$touchpad" |\
			grep -i 'natural scrolling enabled (' |\
			sed -E "$propidRe")

xinput set-prop "$touchpad" "$tappingToggle" 0
xinput set-prop "$touchpad" "$clickMethod" 0 1
xinput set-prop "$touchpad" "$naturalScrolling" 1

### Power saving:

# Disable power saving.  I make the computer sleep when I don’t use it
# anyways.

# From: https://raspberrypi.stackexchange.com/questions/752/

xset s off         # don't activate screensaver
xset -dpms         # disable DPMS (Energy Star) features.
xset s noblank     # don't blank the video device
