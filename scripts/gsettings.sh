#!/bin/sh
# gsettings.sh --- Set Gnome desktop settings.

# Make CapsLock a tertiary Control button.
gsettings set org.gnome.desktop.input-sources xkb-options "['caps:ctrl_modifier']"
# Touchpad clicks: 1 finger: primary, 2 fingers: secondary, 3 fingers:
# middle click.
gsettings set org.gnome.desktop.peripherals.touchpad click-method 'fingers'
# Emacs key theme
gsettings set org.gnome.desktop.interface gtk-key-theme "Emacs"

# Sane Alt-Tab: switch between windows (not apps) in the current workspace.
gsettings set org.gnome.desktop.wm.keybindings switch-windows '["<Alt>Tab"]'
gsettings set org.gnome.shell.window-switcher current-workspace-only "true"

# Consistent window focus and raising: just don't.
gsettings set org.gnome.desktop.wm.preferences focus-new-windows strict

# Tell the user that this script was run.
touch ~/.cf-scripts-gsettings-sh-was-applied
