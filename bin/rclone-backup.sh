#!/bin/sh
# rclone-backup.sh --- rclone backup script

. $HOME/.profile
. $MYLIB/cron.sh
. $MYLIB/fns.sh

say starting rclone backup...

sh "$MYSYSTEM/online-backup.sh"  2>&1

notify-send -u low 'rclone backup completed' 'Remote backup successfully completed'

say 'done completed finished bye'

