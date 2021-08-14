#!/bin/sh
# take-a-break.sh --- break reminder

export GK_NOENV=yes
. $HOME/.profile
. $MYLIB/cron.sh

notify-send -u critical -i face-glasses \
    'hey, take a break maybe?'          \
    "stand up for five minutes, you dork. It's good for your body."
