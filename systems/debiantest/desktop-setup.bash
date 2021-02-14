# desktop-setup.bash --- initialisation for desktop session

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

export DESKTOP_SESSION=i3wm
export GK_COLOUR_SCHEME_PREFERENCE=light

### Start background processes:
/usr/lib/geoclue-2.0/demos/agent & # required for access to geoclue2
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
