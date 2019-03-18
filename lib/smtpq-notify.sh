#!/bin/bash
# smtpq-notify.sh --- send a notification if there is queued outbound mail

. $HOME/cf/lib/cron.sh

if [ "x$GK_CRON_NO_DBUS" = "xyes" ]; then
    # Nothing to do if we can’t access dbus.
    exit
fi

# Load prompt.bash for bp_queue
. $HOME/cf/lib/prompt.bash

icon="/usr/share/icons/Adwaita/scalable/status/mail-unread-symbolic.svg"

msg="$(bp_queue)"

if [ -n "$msg" ]; then
    notify-send -i "$icon" "$msg"
fi
