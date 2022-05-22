#!/usr/bin/env bash
# remind-bujo.bash --- send reminder notification to check bujo

export GK_NOENV=yes
. $HOME/.profile
. $MYLIB/cron.sh

# bash strict mode
set -euo pipefail

icon="/usr/share/icons/Adwaita++/apps/scalable/bookworm.svg"
if [ -f "$icon" ]; then
    icon="-i $icon"
else
    icon=""
fi

notify-send -u critical $icon \
            "Bullet journala baktın mı?" \
            "Ne yapıyorsun? Şu anda ne yapmayı planlamıştın?"
