# batmon.sh --- cron battery monitor

. $HOME/cf/lib/cron.sh

batdir=/sys/class/power_supply/BAT0/
. $batdir/uevent

if [ "Discharging" != "$POWER_SUPPLY_STATUS" ]; then
    exit
else
    low="$(( $POWER_SUPPLY_ENERGY_FULL / 100 * 25 ))"
    if [ $POWER_SUPPLY_ENERGY_NOW -lt $low ]; then
        notify-send -u critical "Low Battery" \
                    "Battery level too low, consider connecting to power source"
    fi
fi
