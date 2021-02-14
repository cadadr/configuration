# desktop-setup.bash --- initialisation for desktop session

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

export DESKTOP_SESSION=i3wm
export GK_COLOUR_SCHEME_PREFERENCE=light

### Start background processes:
dunst                    &
redshift-gtk             &
kdeconnect-indicator     &
clipit                   &
# volctl                   &
xsettingsd               &
flameshot                &
nm-applet                &
pcmanfm --daemon-mode    &
# gpg-agent, ssh-agent?

exec dbus-launch --exit-with-session i3
