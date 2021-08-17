#!/bin/sh
# rclone-backup.sh --- rclone backup script

export GK_NOENV=yes
. $HOME/.profile
. $MYLIB/cron.sh
. $MYLIB/fns.sh

say starting rclone backup...

rclone copy $HOME/Documents encryptifier:/Documents 2>&1
rclone copy $HOME/Zotero    encryptifier:/Zotero 2>&1
rclone copy $HOME/.zotero   encryptifier:/ZoteroConfig 2>&1
rclone copy $HOME/fil       encryptifier:/Misc 2>&1
rclone copy $HOME/.mozilla  encryptifier:/Mozilla 2>&1
rclone copy $HOME/.config/chromium  encryptifier:/Chromium 2>&1

say 'done completed finished bye'

