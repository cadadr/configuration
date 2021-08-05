#!/bin/sh
# dconf-dump.sh --- dump dconf database for system

. $MYLIB/fns.sh

pidfile="$HOME/.dconf-dump.sh.pid"

if [ -e "$pidfile" ]; then
    kill "$(cat $pidfile)"
fi

echo $$ > $pidfile

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
refresh-last-run
history-search-for
installed-apps
document-directory
selected-files
current-folder
window-size
maximized
last-viewed-location
transportation-type
window-maximized
sidebar-width
sidebar-size
save-directory
show-hidden-files
size
state
window-state
window-height
window-width
device-aliases
numlock-state
volume
volume-sound-enabled
num-workspaces
location-mode
last-node-selected
last-window-state
saved-view
selected-color
custom-colors
last-active-tool
last-right-rgba
last-size
last-left-rgba
current-tab
window-pane-position
recent-emoji
sort-col
picture-uri
ignore-ca-cert
ignore-phase2-ca-cert
last-char
favorite-apps-list
looking-glass-history
saved-pathbar-path
night-light-last-coordinates
last-refresh-time
last-active-shape
last-font-name
sort-order
sort-column
col-[0-9]+-width
api-caches
focused-document-view[0-9]+
opened-files-view[0-9]+
last-folder-uri
tab-info-list
tabs
position
my-computer-expanded"

# This pops up sometimes for some reason
if [ "$DESKTOP_SESSION" = "cinnamon" ]; then
    dconf reset -f /org/mate/
fi

say "started dconf dumper..."

while true; do
    sleep 360s
    say "dumping dconf database for $MYSYSTEM..."
    /usr/bin/dconf dump / \
	| grep -Ev "^($(echo $filter | tr ' ' '|'))=" \
	| grep -v 'nm-applet' \
	| inirefmt.py \
	       > $MYSYSTEM/dconf.dump
    say 'done'
done
