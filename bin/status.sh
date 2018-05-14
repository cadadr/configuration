#!/bin/sh
# status.sh --- show system status

. /sys/class/power_supply/BAT0/uevent

power="$POWER_SUPPLY_STATUS (%$POWER_SUPPLY_CAPACITY)"

ip=$(hostname -I | cut -d\  -f 1)
host=$(hostname -f)
user="$USER@$host ($ip)"

msg="$user\nBattery: $power"

if [ -n "$DISPLAY" ]; then
        exec notify-send "$(date)" "$msg"
else
	date
	echo $msg
fi

