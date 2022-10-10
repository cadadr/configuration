#!/usr/bin/env bash
# remind-bujo.bash --- send reminder notification to check bujo

. $HOME/.profile
. $MYLIB/cron.sh

icon="/usr/share/icons/Adwaita++/apps/scalable/bookworm.svg"
if [ -f "$icon" ]; then
    icon="-i $icon"
else
    icon=""
fi

if [ -n "$DISPLAY" ]; then
    notify-send -u critical $icon \
		"Bullet journala baktın mı?" \
		"Ne yapıyorsun? Şu anda ne yapmayı planlamıştın?"
fi
