#!/usr/bin/env bash
# borg-backup.bash --- create backups w/ borg

export GK_NOENV=yes
. $HOME/.profile
. $MYLIB/cron.sh
. $MYLIB/fns.sh

# If ‘GK_BORG_INTERACTIVE’ is set, display progress info.
maybe_progress="${GK_BORG_INTERACTIVE:+--stats --progress}"

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

exit_code=$?
if [ "$exit_code" -eq 0 ]; then
    notify-send -u low \
                'Borg backup completed' 'Local backup successfully completed'
    say done finished completed ciao
else
    notify-send -u critical \
                "Borg backup failed with error code ${exit_code}!"
    say failed with $exit_code
fi
