# desktop-setup.bash --- desktop session for sappho

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

### Environment:
export DESKTOP_SESSION=i3wm
# Let’s pretend to be XFCE4.
export XDG_CURRENT_DESKTOP=XFCE
export GK_COLOUR_SCHEME_PREFERENCE="${GK_COLOUR_SCHEME_PREFERENCE:-light}"
export GK_XBGIMG="$HOME/.xbg.png"

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

### App settings:

# Behave bitch. Fuck you, Mozilla. Choke on a bucket of dicks.
export MOZ_USE_XINPUT2=1

### X settings:

# Disable bell
xset -b

# Use XToolkit in java applications
export AWT_TOOLKIT=XToolkit

# Enable core dumps in case something goes wrong
ulimit -c unlimited


### Configure I/O:

bash $MYLIB/hw/keyboard.bash

bash $MYLIB/hw/tpx230.bash

# If the USB trackball is attached, configure it.
xinput list | grep "Logitech USB Trackball" >/dev/null 2>&1 \
    && bash $MYLIB/hw/logitech_trackman.bash


### Start background processes:
mate-power-manager       &
dunst                    &
kdeconnect-indicator     &
clipit                   &
if which pipewire >/dev/null 2>/dev/null; then
    pipewire             &
    pipewire-pulse       &
fi
# if flameshot starts too quickly, the icon doesn't go to the
# system tray.
(sleep 5; flameshot)     &
nm-applet                &
(sleep 2; volctl)        &
blueman-applet           &
picom --config $MY/dotfiles/picom.conf &
# Activate script on suspend.
xss-lock -l -- $HOME/bin/lockscr.sh &
# Disk automounter.
udiskie -t &
# Polkit authentication agent.
/usr/libexec/xfce-polkit &

### Set up themes and visuals:

# Desktop background

if [ -f "$GK_XBGIMG" ]; then
    feh --no-fehbg --bg-fill "$GK_XBGIMG"
else
    echo No image found at "$GK_XBGIMG". Cannot set desktop background.
fi

# Utilities

gk_dark_theme_p () {
    [ "$GK_COLOUR_SCHEME_PREFERENCE" = "dark" ]
    return $?
}

gk_light_theme_p () {
    [ "$GK_COLOUR_SCHEME_PREFERENCE" = "light" ]
    return $?
}

# Variables
GK_GTK3_SETTINGS_FILE="$MY/dotfiles/gtk-3.0/settings.ini"

if gk_dark_theme_p; then
    mouse_theme=posy-black
elif gk_light_theme_p; then
    mouse_theme=posy-white
fi

mouse_size=16

# GTK
cat <<EOF > "$GK_GTK3_SETTINGS_FILE"
[Settings]
gtk-cursor-theme-name = ${mouse_theme}
gtk-cursor-theme-size = ${mouse_size}
gtk-decoration-layout = menu:close
EOF

if gk_dark_theme_p; then
    echo 'gtk-application-prefer-dark-theme = true' >> "$GK_GTK3_SETTINGS_FILE"
fi

# final newline
echo >> "$GK_GTK3_SETTINGS_FILE"

# Gsettings
gsettings set org.gnome.desktop.interface cursor-theme "$mouse_theme"
gsettings set org.gnome.desktop.interface cursor-size  "$mouse_size"
gsettings set org.gnome.desktop.interface color-scheme "prefer-$GK_COLOUR_SCHEME_PREFERENCE"

# Qt
export QT_STYLE_OVERIDE="adwaita"

# Mouse theme
mkdir -p $HOME/.icons/default
cat <<EOF > "$HOME/.icons/default/index.theme"
[icon theme]
Inherits=${mouse_theme}
EOF

# i3wm
if gk_dark_theme_p; then
    echo "include colours_gruvbox" > "$HOME/.config/i3/colours_active_theme"
elif gk_light_theme_p; then
    echo "include colours_my_blue" > "$HOME/.config/i3/colours_active_theme"
fi

# Individual apps
if gk_dark_theme_p; then
    export GK_ROFI_THEME=/usr/share/rofi/themes/gruvbox-dark-hard.rasi
    export GK_KITTY_THEME=colours_gruvbox_dark.conf
elif gk_light_theme_p; then
    export GK_ROFI_THEME=/usr/share/rofi/themes/Indego.rasi
    export GK_KITTY_THEME=colours_tomorrow_light.conf
fi

# Theming related environment variables
export XCURSOR_THEME="$mouse_theme"
export XCURSOR_SIZE=$mouse_size

### Launch window manager:
# Detect if docked. Because udev is a pile of crap, I need to do this
# manually... d'uh..
(cat /sys/devices/platform/dock.*/docked | grep 1) >/dev/null \
    && $MYSYSTEM/dock.bash docked

(sleep 10; notify-send -t 2000 welcome "welcome to $(hostname)!" ) &

# dotfiles/xsession will run $MYSYSTEM/desktop-setup.bash with
# dbus-launch, so we can just run i3 here.
exec i3

