#!/bin/bash
# make-backup.sh --- make a tarball of /igk

set -e

OUTDIR=${OUTDIR:=/backups}
IGKDIR=${IGKDIR:=$MYFS}

ballname="backup.$(date --utc +'%F.h%H%M.%Z')"
ballext="tar.xz"
ball="$ballname.$ballext"

files="$(find /igk -maxdepth 1 				\
	      -not \( -name '.*' -o -name 'lost+found'	\
	      -o -name 'backup.*' -o -name 'tmp' \) 	)"

# Count the number of files to be backed up, includes directories too.
count="$(find /igk/ | wc -l)"

# Total size of the input.
insiz="$(du -hs $files | cut -f 1)"

tarcmd="tar --force-local --preserve-permissions 		\
	    --acls --selinux --xattrs --create --xz --verbose 	\
	    --file=$OUTDIR/$ball $(echo $files)"

pvcmd="pv --timer --eta --progress --size $count --line-mode -"

echo "Will back up $count items, $insiz of data in total..."
echo "Input directory: $IGKDIR"
echo "Output file: $OUTDIR/$ball"
echo

$tarcmd | $pvcmd >/dev/null

chmod 400 "$OUTDIR/$ball"

echo "Write MD5SUM..."
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
