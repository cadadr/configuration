# desktop-setup.bash --- initialisation for desktop session

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

### Environment:
export DESKTOP_SESSION=i3wm
export GK_COLOUR_SCHEME_PREFERENCE=dark

# Manually set lat and long from zone.tab, geoclue is unreliable
# because FUCK GNOME FUCK GNOME FUCK GNOME.
latlong="$(grep $TZ /usr/share/zoneinfo/zone.tab | awk '{print $2}')"
_lat="$(echo $latlong | cut -d+ -f2)"
_long="$(echo $latlong | cut -d+ -f3)"

export LOCATION_LAT="$(echo 2 k $_lat 100 / p | dc)"
export LOCATION_LONG="$(echo 2 k $_long 100 / p | dc)"

unset _lat _long latlong

# Disable DPMS turning off the screen
xset -dpms
xset s off

# Disable bell
xset -b

# Swap mouse back/forward buttons around.
xinput set-button-map "USB Optical Mouse" 1 2 3 4 5 6 7 9 8

# Hold middle button to scroll in all directions.
xinput set-prop "USB Optical Mouse" \
	'libinput Scroll Method Enabled' {0,0,1}

# Use XToolkit in java applications
export AWT_TOOLKIT=XToolkit

# Enable core dumps in case something goes wrong
ulimit -c unlimited


### Start background processes:
dunst                    &
redshift-gtk -l $LOCATION_LAT:$LOCATION_LONG &
kdeconnect-indicator     &
clipit                   &
# if flameshot starts too quickly, the icon doesn't go to the
# system tray.
(sleep 5; flameshot)     &
nm-applet                &
pcmanfm --daemon-mode    &
pasystray                &
# gpg-agent, ssh-agent?

exec dbus-launch --exit-with-session i3 -V -d all
