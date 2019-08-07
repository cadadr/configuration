#!/bin/bash
# make-backup.sh --- make a tarball of /igk

set -e

. $MYLIB/fns.sh

OUTDIR=${OUTDIR:=/backups}
IGKDIR=${IGKDIR:=$MYFS}
SERVICES="pulseaudio $(cd $HOME/.config/systemd/user && ls *.service | cut -d. -f1)"

WAIT_SECS=${WAIT_SECS:=1}

ballname="backup.$(date --utc +'%F.h%H%M.%Z')"
ballext="tar.xz"
ball="$ballname.$ballext"

files="$(find /igk -maxdepth 1 				\
	      -not \( -name '.*' -o -name 'lost+found'	\
	      -o -name 'backup.*' -o -name 'tmp' \) 	)"

say Count the number of files to be backed up, includes directories too...
count="$(find /igk/ | wc -l)"

say Calculate total size of the input...
insiz="$(du -hs $files | cut -f 1)"

tarcmd="tar --force-local --preserve-permissions 		\
	    --acls --selinux --xattrs --create --xz --verbose 	\
	    --file=$OUTDIR/$ball $(echo $files)"

pvcmd="pv --timer --eta --progress --size $count --line-mode -"

say Close programs that keep $IGKDIR busy:

for service in $SERVICES; do
    if systemctl --user is-active --quiet $service; then
        say Stop $service.service...
        systemctl --user stop $service
    fi
done

for xpid in $(lsof -Fp "$IGKDIR" | egrep ^p); do
    pid=${xpid/p}               # remove p prefix that lsof adds

    [ "$pid" = "$$" ] && continue

    # Safeguard for when some of the pids were the child of another one
    if [ -e /proc/$pid ]; then
        say Kill $(ps -q "$pid" -o comm=) \($pid\) with SIGTERM...

        kill -TERM $pid

        # Wait for the process to terminate
        # From https://stackoverflow.com/a/19396161
        #
        # XXX(2019-07-20): may be Linux specific, but
        # https://www.freebsd.org/doc/en/articles/linux-users/procfs.html
        # says that procfs can be used in at least FreeBSD too.
        while [ -e /proc/$pid ]; do
            sleep $WAIT_SECS
            # Report if will wait more
            [ -e /proc/$pid ] && say waiting...
        done
 
        say "done"
    fi
done

say Remount $IGKDIR as read-only \(will use sudo\)...
sudo mount -o remount,ro $IGKDIR
mount | grep $IGKDIR

say Remove oldest of backups...
rm -i "$(ls -t $OUTDIR/*.xz | tac | head -1)"*

echo
echo "Will back up $count items, $insiz of data in total..."
echo "Input directory: $IGKDIR"
echo "Output file: $OUTDIR/$ball"
echo
echo Hit Enter to continue
read

say Produce full backup:
$tarcmd | $pvcmd >/dev/null

chmod 400 "$OUTDIR/$ball"

say "Write MD5SUM..."
( cd $OUTDIR && md5sum $ball > $ball.md5sum )

echo
echo
echo ',-----------------------------8<------------------------------.'
echo '|                                                             |'
echo '|                                                             |'
echo '|--------============ BACKUP COMPLETE =============-----------|'
echo '|                                                             |'
echo "The backup ($OUTDIR/$ball) is now complete ($(date))." \
	| fmt -59 | awk '{printf("| %-59s |\n",$0)}'
echo '|                                                             |'
echo "The filesystem mounted at $IGKDIR was remounted as readonly."   \
     'Run the following command in order to remount it as readwrite:' \
	| fmt -59 | awk '{printf("| %-59s |\n",$0)}'
echo '|                                                             |'
if type -t remount; then
echo '    'remount rw $IGKDIR
else
echo '    'sudo mount -o remount,rw $IGKDIR
fi | fmt -59 | awk '{printf("| %-59s |\n",$0)}'
echo '|                                                             |'
echo '| But consider rebooting instead of that.                     |'
echo '|                                                             |'
echo '`----------------------------->8------------------------------'\'
echo
