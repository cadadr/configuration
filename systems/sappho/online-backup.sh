#!/bin/sh
# online-backup.sh --- online backups for sappho

# To be called by bin/rclone-backup.sh

# Set GK_RCLONE_FLAGS to pass flags to rclone(1).

remote="encryptifier-$(hostname)"
flags="$GK_RCLONE_FLAGS"

rclone $flags copy "$HOME/Documents" "$remote:/Documents"
rclone $flags copy "$HOME/Research"  "$remote:/Research"
rclone $flags copy "$HOME/Notlar"    "$remote:/Notlar"
rclone $flags copy "$HOME/Library"   "$remote:/Library"
