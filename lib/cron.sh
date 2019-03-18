# cron.sh --- common setup for cron scripts

# Find dbus
i3pid="$(pgrep -u $LOGNAME i3\$)"

if [ -n "$i3pid" ]; then
    dbus="$(egrep -z DBUS < /proc/$i3pid/environ | tr -d '\0')"
    eval "export $dbus"
else
    export GK_CRON_NO_DBUS=yes
fi

# Load profile environment
. $HOME/cf/lib/env.sh
