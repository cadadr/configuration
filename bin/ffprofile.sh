#!/bin/sh

grep ^Name= ~/.mozilla/firefox/profiles.ini | cut -d= -f2 \
    | rofi -dmenu -p 'Select Firefox profile:' \
    | xargs -r firefox -allow-downgrade -P

