#!/usr/bin/env bash
# borg-backup.bash --- create backups w/ borg

. $HOME/.profile
. $MYLIB/cron.sh
. $MYLIB/fns.sh

# if stdout is not a tty, output to log
if [ ! -t 1 ]; then
    exec >>$MYLOGS/borg.log
    exec 2>&1
fi

check-router-mac-address.bash "$GK_SAPPHO_HOME_ROUTER_MAC" || {
    say "unexpected router MAC, will not run on this network!"
    exit
}

# If ‘GK_BORG_INTERACTIVE’ is set, display progress info.
maybe_progress="${GK_BORG_INTERACTIVE:+--stats --progress}"

say repository: $BORG_REPO

say starting local backup with borg...

if [ ! -f "$MYSYSTEM/full-backup.dirs" ]; then
    die backups not supported, "$MYSYSTEM/full-backup.dirs" not found
fi

borg create $maybe_progress --compression=lz4 "::{user}-{now}" \
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
