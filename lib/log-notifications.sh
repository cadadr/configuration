#!/bin/sh
# log-notifications.sh --- dunst script to log notifications

mkdir -p $HOME/log

cat <<EOF >> $MYLOGS/dunst.log
-----
id:             $DUNST_ID
timestamp:      $DUNST_TIMESTAMP
summary:        $DUNST_SUMMARY
app_name:       $DUNST_APP_NAME
--
body:           $DUNST_BODY
icon_path:      $DUNST_ICON_PATH
urgency:        $DUNST_URGENCY
progress:       $DUNST_PROGRESS
category:       $DUNST_CATEGORY
stack_tag:      $DUNST_STACK_TAG
urls:           $DUNST_URLS
timeout:        $DUNST_TIMEOUT
desktop_entry:  $DUNST_DESKTOP_ENTRY
stack_tag:      $DUNST_STACK_TAG
-----
EOF
