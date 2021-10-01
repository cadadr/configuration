#!/bin/sh
# passmenu.sh -- rofi script for pass(1).

set +e

prefix=${PASSWORD_STORE_DIR-~/.password-store}
prompt="Select password store enty"
password="$(pass git ls-files | grep -v ^\\. | sed s/.gpg\$// \
                 | rofi -dmenu -p "$prompt" -i)"
notify="notify-send -i dialog-password"

[ -z "$password" ] && {
    $notify "empty string received from rofi"
    exit 1
}

pass show -c $password

[ "$?" = "0" ] && $notify "“$password” copied" \
  "Password copied to clipboard, will be cleared after\
 ${PASSWORD_STORE_CLIP_TIME:-45} seconds" \
 || {
    $notify "error copying password '$password'"
    exit 1
}

