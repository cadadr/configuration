# cron.sh --- common setup for cron scripts

# Find dbus
dbus="$(egrep -z DBUS < /proc/$(pgrep -u $LOGNAME i3\$)/environ | tr -d '\0')"
eval "export $dbus"

# Load profile environment
. $HOME/cf/lib/env.sh
