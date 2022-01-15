#!/usr/bin/env bash
# borg-backup.bash --- create backups w/ borg

export GK_NOENV=yes
. $HOME/.profile
. $MYLIB/cron.sh
. $MYLIB/fns.sh

# If ‘GK_BORG_NONINTERACTIVE’ is set, don’t display progress info.
maybe_progress="${GK_BORG_NONINTERACTIVE---stats --progress}"

# repository
user="${GK_BORG_USER-pi}"
host="${GK_BORG_HOST-xanthippe.local}"
dir="${GK_BORG_DIR-/home/$user/cadadr-encrypted-storage/repo-$(hostname)}"
repo=$user@$host:$dir

say repository: $repo

say starting local backup with borg...

if [ ! -f "$MYSYSTEM/full-backup.dirs" ]; then
    die backups not supported, "$MYSYSTEM/full-backup.dirs" not found
fi

borg create $maybe_progress --compression=lz4 \
     $repo::{user}-{now}_initial-archive      \
     $(bash $MYSYSTEM/full-backup.dirs)

notify-send -u low 'Borg backup completed' 'Local backup successfully completed'

say done finished completed ciao
