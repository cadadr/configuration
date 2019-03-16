#!/bin/bash
# smtpq-notify.sh --- send a notification if there is queued outbound mail

. $HOME/cf/lib/cron.sh

# Load prompt.bash for bp_queue
. $HOME/cf/lib/prompt.bash

icon="/usr/share/icons/Adwaita/scalable/status/mail-unread-symbolic.svg"

msg="$(bp_queue)"

if [ -n "$msg" ]; then
    notify-send -i "$icon" "$msg"
fi
