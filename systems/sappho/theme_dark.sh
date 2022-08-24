#!/bin/sh
# theme_dark.sh --- dark theme setup for sappho

GK_GTK3_SETTINGS_FILE="$MY/dotfiles/gtk-3.0/settings.ini"
mouse_theme=posy-black

# GTK
cat <<EOF > "$GK_GTK3_SETTINGS_FILE"
[Settings]
gtk-application-prefer-dark-theme = true
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
echo "include colours_gruvbox" > "$HOME/.config/i3/colours_active_theme"

# Individual apps
export GK_ROFI_THEME=/usr/share/rofi/themes/gruvbox-dark-hard.rasi
export GK_KITTY_THEME=colours_gruvbox_dark.conf
