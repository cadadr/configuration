#!/bin/sh
# dconf-dump.sh --- dump dconf database for system

. $MYLIB/fns.sh

pidfile="$HOME/.dconf-dump.sh.pid"

if [ -e "$pidfile" ]; then
    kill "$(cat $pidfile)"
fi

echo $$ > $pidfile

# This pops up sometimes for some reason
if [ "$DESKTOP_SESSION" = "cinnamon" ]; then
    dconf reset -f /org/mate/
fi

say "started dconf dumper..."

while true; do
    sleep 360s
    . $MYLIB/dconf.filter
    say "dumping dconf database for $MYSYSTEM..."
    /usr/bin/dconf dump / \
	| grep -Ev "^($(echo $filter | tr ' ' '|'))=" \
	| grep -v 'nm-applet' \
	| inirefmt.py \
	       > $MYSYSTEM/dconf.dump
    say 'done'
done
