#!/bin/sh
# passmenu.sh -- dmenu script for pass(1).

set +e

prefix=${PASSWORD_STORE_DIR-~/.password-store}
prompt="Select password store enty"
password=$(pass list | tail -n+2 | cut -d\  -f 2 | dmenu -p "$prompt :" -i)

if [ x$password = x ]; then exit 1; fi

pass show -c "$password"
notify-send "“$password” copied" \
  "Password copied to clipboard, will be cleared after\
 ${PASSWORD_STORE_CLIP_TIME:-45} seconds"
