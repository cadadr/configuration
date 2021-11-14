#!/bin/sh
# rclone-backup.sh --- rclone backup script

export GK_NOENV=yes
. $HOME/.profile
. $MYLIB/cron.sh
. $MYLIB/fns.sh

say starting rclone backup...

rclone copy $HOME/Documents                 encryptifier:/Documents 2>&1
rclone copy $HOME/Research                  encryptifier:/Research  2>&1
rclone copy $HOME/Notes                     encryptifier:/Notes     2>&1

notify-send -u low 'rclone backup completed' 'Remote backup successfully completed'

say 'done completed finished bye'

