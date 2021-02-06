#!/usr/bin/env bash
# desktop-setup.bash --- initialisation for desktop session

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

### Start background processes:
dunst                    &
redshift-gtk             &
kdeconnect-indicator     &
clipit                   &
xfce4-pulseaudio-plugin  &
pavucontrol              &
xsettingsd               &
flameshot                &
# gpg-agent, ssh-agent?
