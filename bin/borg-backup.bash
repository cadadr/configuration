#!/usr/bin/env bash
# borg-backup.bash --- create backups w/ borg

export GK_NOENV=yes
. $HOME/.profile
. $MYLIB/cron.sh
. $MYLIB/fns.sh

say starting local backup with borg...

say 'environment looks like:'

env | grep -i borg

borg create --stats --progress --compression lz4 ::{user}-{now} $MYFS/

notify-send -u low 'Borg backup completed' 'Local backup successfully completed'

say done finished completed ciao
