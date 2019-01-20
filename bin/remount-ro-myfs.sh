#!/bin/sh
# remount-ro-myfs.sh --- remount $MYFS as readonly

# To be used before backups.

cd /

if lsof -p ^$$ $MYFS >/dev/null; then
    # The `-p $$' filters out the PID of this script, or else it commits
    # suicide.  Interesting that Dash does not read the script all at
    # once and close the file (apparently).
    lsof -p ^$$ $MYFS | sed 1d | cut -d ' ' -f2 | xargs kill 
fi

echo Remount $MYFS...
sudo mount -o remount,ro $MYFS
