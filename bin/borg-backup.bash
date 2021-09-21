#!/usr/bin/env bash
# borg-backup.bash --- create backups w/ borg

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

export GK_NOENV=yes
. $HOME/.profile
. $MYLIB/cron.sh
. $MYLIB/fns.sh

say starting local backup with borg...

say 'environment looks like:'

env | grep -i borg

borg create --stats --progress --compression lz4 ::{user}-{now} $MYFS/

say done finished completed ciao
