#!/usr/bin/env bash
# check-router-mac-address.bash --- check if MAC of router is as expected

# Adapted from: https://superuser.com/a/1214259

. $MYLIB/fns2.sh
script_name="$(basename $0)"
script_usage="MAC_ADDRESS"

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

expected_mac="${1-}"

if [ $# -ne 1 ] || [ -z "$expected_mac" ]; then
    exit_with_usage
fi

# Same IP might appear multiple times e.g. if you're connected both my
# WiFi and Ethernet to the same router.
gateway_ip="$(route -n | grep -e '^0\.0\.0\.0' | tr -s ' ' | cut -d ' ' -f 2 | sort | uniq)"

if [ -z "$gateway_ip" ]; then
    exit_with_error_message 2 could not determine gateway IP address
fi

gateway_data=($(arp -n "$gateway_ip" | grep -e "$gateway_ip" | tr -s ' ' \
                    | tr ' ' '\t'))

if [[ "${gateway_data[1]}" == "(incomplete)" ]]; then
    exit_with_error_message 3 gateway data incomplete
elif [[ "${gateway_data[2]}" == "--" ]]; then
    exit_with_error_message 3 could not determine gateway MAC address
elif [[ "${gateway_data[2]}" == "$expected_mac" ]]; then
    exit
else
    exit_with_error_message 1 mismatching MAC addresses: expected \
                            $expected_mac, got "${gateway_data[2]}"
fi
