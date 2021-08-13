#!/bin/sh
# rclone-backup.sh --- rclone backup script

export GK_NOENV=yes
. $HOME/.profile
. $MYLIB/cron.sh
. $MYLIB/fns.sh

say starting rclone backup...

rclone copy $HOME/Documents encryptifier:/Documents
rclone copy $HOME/Zotero    encryptifier:/Zotero
rclone copy $HOME/.zotero   encryptifier:/ZoteroConfig
rclone copy $HOME/fil       encryptifier:/Misc
rclone copy $HOME/.mozilla  encryptifier:/Mozilla
rclone copy $HOME/.config/chromium  encryptifier:/Chromium

say 'done completed finished bye'

