# desktop-setup.bash --- desktop session for sappho

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

eval $(ssh-agent -s)
GPG_TTY=$(tty)
export GPG_TTY

# Ensure PulseAudio DBus service is running.
start-pulseaudio-x11

### X settings:

# Disable bell
xset -b

# Use XToolkit in java applications
export AWT_TOOLKIT=XToolkit

# Enable core dumps in case something goes wrong
ulimit -c unlimited


### Hardware settings
# Disable the annoying ThinkPad touchpad
xinput set-prop 'SynPS/2 Synaptics TouchPad' 'Device Enabled' 0

### Start background processes:
setbg.bash               &
dunst                    &
kdeconnect-indicator     &
clipit                   &
pipewire                 &
pipewire-pulse           &
# if flameshot starts too quickly, the icon doesn't go to the
# system tray.
(sleep 5; flameshot)     &
nm-applet                &
volctl                   &
blueman-applet           &
picom --config $MY/dotfiles/picom.conf &
# Activate script on suspend.
xss-lock -l -- $HOME/bin/lockscr.sh &
# Disk automounter.
udiskie -t &

### Set up toolkit looks:
GK_GTK3_SETTINGS_FILE="$MY/dotfiles/gtk-3.0/settings.ini"

theme_dark(){
    mouse_theme=posy-black
    cat <<EOF > "$GK_GTK3_SETTINGS_FILE"
[Settings]
gtk-application-prefer-dark-theme = true
gtk-cursor-theme-name = ${mouse_theme}

EOF
    export QT_STYLE_OVERRIDE="Fusion"
    cat <<EOF > "$HOME/.icons/default/index.theme"
[icon theme]
Inherits=${mouse_theme}
EOF

    echo "include colours_gruvbox" > "$HOME/.config/i3/colours_active_theme"

    export GK_ROFI_THEME=/usr/share/rofi/themes/gruvbox-dark-hard.rasi

    export GK_KITTY_THEME=colours_gruvbox_dark.conf
}

theme_light(){
    mouse_theme=posy-white
    cat <<EOF > "$GK_GTK3_SETTINGS_FILE"
[Settings]
gtk-application-prefer-dark-theme = false
gtk-cursor-theme-name = ${mouse_theme}

EOF
    export QT_STYLE_OVERRIDE="Fusion"
    cat <<EOF > "$HOME/.icons/default/index.theme"
[icon theme]
Inherits=${mouse_theme}
EOF

    echo "include colours_my_blue" > "$HOME/.config/i3/colours_active_theme"

    export GK_ROFI_THEME=/usr/share/rofi/themes/Indego.rasi

    export GK_KITTY_THEME=colours_atelier_cave_light.conf
}

theme_$GK_COLOUR_SCHEME_PREFERENCE

echo 'gtk-decoration-layout = menu:close' >> $GK_GTK3_SETTINGS_FILE


### Launch window manager:
(sleep 10; notify-send welcome "welcome to $(hostname)!" ) &

# dotfiles/xsession will run $MYSYSTEM/desktop-setup.bash with
# dbus-launch, so we can just run i3 here.
exec i3 -V

