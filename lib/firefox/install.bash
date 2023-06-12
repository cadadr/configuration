#!/usr/bin/env bash
# install.bash --- install settings to all firefox profiles

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

profilesdir=$HOME/.mozilla/firefox

grep ^Path $profilesdir/profiles.ini \
     | while read line
do
    profile_id="${line#Path=}"
    profile_path="$profilesdir/$profile_id"
    if [ -d $profile_path ]; then
        css_dest=$profile_path/chrome/userChrome.css
        js_dest=$profile_path/user.js

        mkdir -pv $profile_path/chrome

        install -v -T --backup=numbered $MYLIB/firefox/userChrome.css $css_dest
        install -v -T --backup=numbered $MYLIB/firefox/user.js $js_dest
    fi
done
