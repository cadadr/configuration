# desktop-setup.bash --- initialisation for desktop session

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

### Environment:
export DESKTOP_SESSION=i3wm
export GK_COLOUR_SCHEME_PREFERENCE=dark

# Disable DPMS turning off the screen
xset -dpms
xset s off

# Disable bell
xset -b

# Swap mouse back/forward buttons around.
xinput set-button-map "USB Optical Mouse" 1 2 3 4 5 6 7 9 8

# Use XToolkit in java applications
export AWT_TOOLKIT=XToolkit

# Enable core dumps in case something goes wrong
ulimit -c unlimited


### Start background processes:
export $(gnome-keyring-daemon --start --components=ssh) &
/usr/lib/geoclue-2.0/demos/agent & # required for access to geoclue2
dunst                    &
# Wait for geoclue agent.
( sleep 10; redshift-gtk ) &
kdeconnect-indicator     &
clipit                   &
# volctl                   &
# xsettingsd               &
# if flameshot starts too quickly, the icon doesn't go to the
# system tray.
(sleep 5; flameshot)     &
nm-applet                &
pcmanfm --daemon-mode    &
# gpg-agent, ssh-agent?

exec dbus-launch --exit-with-session i3 -V -d all
