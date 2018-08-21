#!/bin/sh
# status.sh --- show system status

. /sys/class/power_supply/BAT0/uevent

power="$POWER_SUPPLY_STATUS (%$POWER_SUPPLY_CAPACITY)"

ip=$(hostname -i)
host=$(hostname -f)
user="$USER@$host ($ip)"

date
echo $user
echo Battery: $power
