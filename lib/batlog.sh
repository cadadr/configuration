#!/bin/sh
# batlog.sh --- live-log battery levels info as CSV

. $MYLIB/fns.sh

### Parametres:

# The path to uevents file.
device_file="${DEVICE_FILE-/sys/class/power_supply/BAT0/uevent}"
# Poll granularity.  BEWARE polling too frequently may not only impact
# performance but mess up the storage drive that receives the logs.
poll_interval_secs=60

### Code:

[ -f "$device_file" ] || die file $device_file does not exist

# Print a header.
echo "# Reading power data from \`$device_file',"
echo "# started at $(date)"
echo \#
printf 'unix_time,'
< $device_file cut -d= -f1 | tr '\n' ',' | tr '[:upper:]' '[:lower:]'
echo "load_0,load_1,load_2"

while true; do
    printf "$(date +%s),"
    # Poll data and print.
    < $device_file cut -d= -f2 | tr '\n' ',' | tr '[:upper:]' '[:lower:]'
    uptime | cut -d: -f5 | tr -d ' '

    sleep ${poll_interval_secs}s
done
