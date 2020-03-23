#!/bin/sh
# dump-dconf.sh --- dump dconf database near the db itself

filter="session-start
geometry
window-position
window-ratio
command-history
check-timestamp
last-panel
application-id
virtual-root
extension-cache-updated
refresh-last-run"

/usr/bin/dconf dump / \
    | grep -Ev "^($(echo $filter | tr ' ' '|'))=" \
           > $HOME/cf/lib/dconf.dump
