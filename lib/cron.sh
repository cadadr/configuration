# cron.sh --- common setup for cron scripts

# Find dbus
pid="$(pgrep -u $LOGNAME nm-applet\$)"

if [ -n "$pid" ]; then
    dbus="$(egrep -z DBUS < /proc/$pid/environ | tr -d '\0')"
    eval "export $dbus"
else
    export GK_CRON_NO_DBUS=yes
fi

# Load profile environment
. $HOME/cf/lib/env.sh
