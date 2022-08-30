#!/bin/sh
# dconf-dump.sh --- dump dconf database for system

. $HOME/.profile
. $MYLIB/cron.sh
. $MYLIB/fns.sh

# This pops up sometimes for some reason
if [ "$DESKTOP_SESSION" = "cinnamon" ]; then
    dconf reset -f /org/mate/
fi

# Load filters
. $MYLIB/dconf.filter

say "dumping dconf database for $MYSYSTEM..."
/usr/bin/dconf dump / \
    | grep -Ev "^($(echo $filter | tr ' ' '|'))=" \
    | grep -v 'nm-applet' \
    | inirefmt.py \
	   > $MYSYSTEM/dconf.dump
say 'done'
