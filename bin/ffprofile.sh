#!/bin/sh

grep ^Name= ~/.mozilla/firefox/profiles.ini | cut -d= -f2 \
    | dmenu -p 'Select Firefox profile:' \
    | xargs -r ~/Applications/firefox/firefox -P

