# xcommon.sh --- common Xorg setup

### Environment:
. $MYLIB/fns.sh

# Fool shitty apps to believe that this is a desktop environment.
# Simple Scan won't allow keybindings otherwise.
export XDG_CURRENT_DESKTOP=XFCE

export WM=i3

### Code:
# Inform systemd
systemctl --user import-environment DISPLAY

### X resources:
xrdb -merge $MEINE/Xdefaults

### Input:
#### Keyboard:
setxkbmap -option "ctrl:nocaps"
xmodmap $MYLIB/menu-as-hyper.xmodmap

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

### Visuals:
export XBGIMG=$HOME/pic/wlp/goodfon/some-indonesian-mountains.jpg

# Do not do this here because this can be used by a window manager
# too.  Run this function in xinitrc instead.
apply_xbgimg(){
    feh --bg-scale "$XBGIMG"
}
