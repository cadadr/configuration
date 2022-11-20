#!/bin/sh
# rclone-backup.sh --- rclone backup script

. $HOME/.profile
. $MYLIB/cron.sh
. $MYLIB/fns.sh

# if stdout is not a tty, output to log
if [ ! -t 1 ]; then
    exec >>$MYLOGS/rclone.log
    exec 2>&1
fi

say starting rclone backup...

sh "$MYSYSTEM/online-backup.sh"  2>&1

notify-send -u low 'rclone backup completed' 'Remote backup successfully completed'

say 'done completed finished bye'

