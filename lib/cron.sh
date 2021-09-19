# cron.sh --- common setup for cron scripts

env=/proc/$(pgrep -f "^$SESSION_MANAGER" -u "$USER")/environ

export $(cat $env | egrep -z '^DBUS_SESSION_BUS_ADDRESS=')

