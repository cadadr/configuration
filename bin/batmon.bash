#!/bin/bash
# batmon.bash --- battery monitor script, for cron

. $HOME/.profile
. $MYLIB/cron.sh

path=/sys/class/power_supply
battery=BAT0

# Sourcing here doesn’t work because some values can suddenly contain
# spaces, because programmers are dickheads who care nothing about
# backwards compatibility.
export $(sed 's/ /_/g; s/^/export /' $path/$battery/uevent)

# We use designed power leven instead of actual power level to match
# i3status.
x="$POWER_SUPPLY_ENERGY_NOW"
y="$POWER_SUPPLY_ENERGY_FULL_DESIGN"
percent_full="$(echo 2k $x $y / 100 \*p | dc | cut -d. -f1)"

if [ "$POWER_SUPPLY_STATUS" = 'Discharging' ] \
    && [ "$percent_full" -lt 20 ];
then
    notify-send -u critical -h int:value:"$percent_full"              \
	"Low battery" "Battery level critically low ($percent_full%)" \
	--icon=ac-adapter
fi

