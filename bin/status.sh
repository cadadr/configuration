#!/bin/sh
# status.sh --- show system status

. /sys/class/power_supply/BAT0/uevent

power="$POWER_SUPPLY_STATUS (%$POWER_SUPPLY_CAPACITY)"

ip=$(hostname -I | cut -d\  -f 1)
host=$(hostname -f)
user="$USER@$host ($ip)"

if [ ! x$TERM = xdumb ]; then
    clear
fi

exec notify-send "System status, $(date)" "$user\n\nBattery: $power"
