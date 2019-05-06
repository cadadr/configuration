#!/bin/sh
# dump-dconf.sh --- dump dconf database near the db itself

/usr/bin/dconf dump / \
    | grep -Ev '^(session-start|geometry)=' \
           > $HOME/.config/dconf/user.dump
