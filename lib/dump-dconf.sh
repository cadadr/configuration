#!/bin/sh
# dump-dconf.sh --- dump dconf database near the db itself

/usr/bin/dconf dump / \
    | grep -Ev '^(session-start|geometry|window-position|window-ratio|command-history|check-timestamp)=' \
           > $HOME/cf/lib/dconf.dump
