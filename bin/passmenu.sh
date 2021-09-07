#!/bin/sh
# passmenu.sh -- rofi script for pass(1).

set +e

prefix=${PASSWORD_STORE_DIR-~/.password-store}
prompt="Select password store enty"
password="$(pass git ls-files | grep -v ^\\. | sed s/.gpg\$// \
                 | rofi -dmenu -p "$prompt" -i)"

[ -z "$password" ] && {
    notify-send -u critical "empty string received from rofi"
    exit 1
}

pass show -c $password

[ "$?" = "0" ] && notify-send "“$password” copied" \
  "Password copied to clipboard, will be cleared after\
 ${PASSWORD_STORE_CLIP_TIME:-45} seconds" \
 || {
    notify-send -u critical "error copying password '$password'"
    exit 1
}

