# desktop-setup.bash --- initialisation for desktop session

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

### Environment:
export DESKTOP_SESSION=i3wm
export GK_COLOUR_SCHEME_PREFERENCE=dark
export GK_XBGIMG="$HOME/.xbg.png"
export GK_XBG_SLIDESHOW_DIR="$HOME/Pictures/wlp/slideshow/"
export GK_XBG_SLIDESHOW_INTERVAL_MINS=5

# Manually set lat and long from zone.tab, geoclue is unreliable
# because FUCK GNOME FUCK GNOME FUCK GNOME.
latlong="$(grep $TZ /usr/share/zoneinfo/zone.tab | awk '{print $2}')"
_lat="$(echo $latlong | cut -d+ -f2)"
_long="$(echo $latlong | cut -d+ -f3)"

export LOCATION_LAT="$(echo 2 k $_lat 100 / p | dc)"
export LOCATION_LONG="$(echo 2 k $_long 100 / p | dc)"

unset _lat _long latlong

export $(gnome-keyring-daemon --start)

### X settings:
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
setbg.bash               &
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
compton -co .9 -r 15 -t -20 -l -20  &

bash $MYLIB/launch-session-servers.bash

### Set up toolkit looks:
GK_GTK3_SETTINGS_FILE=$HOME/.config/gtk-3.0/settings.ini

case $GK_COLOUR_SCHEME_PREFERENCE in
    dark) cat <<EOF > $GK_GTK3_SETTINGS_FILE
[Settings]
gtk-application-prefer-dark-theme = true
EOF
          export QT_STYLE_OVERRIDE="adwaita-dark"
          ;;
esac

echo 'gtk-decoration-layout = menu:close' >> $GK_GTK3_SETTINGS_FILE

### Sound:
# Do not automatically disable loud speakers when headphone is plugged
# in. This allows to manually select between them, useful when recording.
amixer -c 1 set 'Auto-Mute Mode' Disabled

### Launch window manager:
(sleep 10; notify-send welcome "welcome to $(hostname -f)!" ) &

# dotfiles/xsession will run $MYSYSTEM/desktop-setup.bash with
# dbus-launch, so we can just run i3 here.
exec i3 -V
