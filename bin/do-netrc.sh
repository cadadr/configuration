#!/bin/sh
# do-netrc.sh --- temporarily expose ~/.netrc and run script.
shmfil=/dev/shm/$USER-netrc
authinfo=~/fil/authinfo.gpg
netrc=~/.netrc

if [ ! -n "$1" ]; then
    echo usage: do-netrc.sh SHELL-SCRIPT
    exit 1
fi

if [ -f $netrc ]; then
    echo "error: $netrc exists, remove it and retry"
    exit 1
fi

touch $shmfil
chmod 600 $shmfil
ln -s $shmfil $netrc

gpg2 -q -d - < $authinfo > $shmfil

( $@ )

rm $shmfil
rm $netrc
