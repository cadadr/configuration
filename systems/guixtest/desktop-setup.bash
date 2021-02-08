# desktop-setup.bash --- initialisation for desktop session

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

### Start background processes:
dunst                    &
redshift-gtk             &
kdeconnect-indicator     &
clipit                   &
volctl                   &
xsettingsd               &
flameshot                &
# gpg-agent, ssh-agent?

exec dbus-launch --exit-with-session i3 2>&1 >> $XERRORS_FILE
