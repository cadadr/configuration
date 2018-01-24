#!/bin/sh
# status of this system.

set -e

say () {
    (
	. /sys/class/power_supply/BAT0/uevent
	
	power="$POWER_SUPPLY_STATUS (%%$POWER_SUPPLY_CAPACITY)"
	
	ip=$(hostname -I | cut -d\  -f 1)
	host=$(hostname -f)
	user="$USER@$host ($ip)"

	if [ ! x$TERM = xdumb ]; then
	    clear
	fi

	printf "$user | Battery: $power | $(date +%c)"
    )
    return 0
}

while say; do
    sleep 5
done
