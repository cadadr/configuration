#!/bin/sh
# theme_light.sh --- light theme setup for sappho

GK_GTK3_SETTINGS_FILE="$MY/dotfiles/gtk-3.0/settings.ini"
mouse_theme=posy-white

# GTK
cat <<EOF > "$GK_GTK3_SETTINGS_FILE"
[Settings]
gtk-application-prefer-dark-theme = false
gtk-cursor-theme-name = ${mouse_theme}
gtk-decoration-layout = menu:close

EOF

# Qt
export QT_STYLE_OVERRIDE="Fusion"

# Mouse
cat <<EOF > "$HOME/.icons/default/index.theme"
[icon theme]
Inherits=${mouse_theme}
EOF

# i3wm
echo "include colours_my_blue" > "$HOME/.config/i3/colours_active_theme"

# Individual apps
export GK_ROFI_THEME=/usr/share/rofi/themes/Indego.rasi
export GK_KITTY_THEME=colours_atelier_cave_light.conf
