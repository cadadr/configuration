#!/usr/bin/env bash
# volset.bash --- set volume and send notification

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

notify() {
    echo $0: $@
    notify-send -t 500 $@
}

setvol(){
    notify volume "$(amixer sset Master $1 | grep '%' | head -1 | awk '{print $5}')"
}

togglemute(){
    amixer sset Master toggle | grep '\[on\]' \
        && notify sound "unmuted $(amixer get Master | grep '%' | head -1 | awk '{print $5}')" \
            || notify sound muted
}

case $1 in
    up)   setvol "5%+" ;;
    down) setvol "5%-" ;;
    toggle-mute) togglemute ;;
    full) setvol "100%" ;;
esac
