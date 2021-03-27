#!/usr/bin/env bash
# setbg.bash --- desktop background slideshow

. $MYLIB/fns.sh

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

slideshow_seconds="$(( ${GK_XBG_SLIDESHOW_INTERVAL_MINS-15} * 60 ))"

background_dir="${GK_XBG_SLIDESHOW_DIR-$HOME/Pictures/wlp}"

xbg="${GK_XBGIMG-$HOME/.xbg.png}"

if [ ! -d "$background_dir" ]; then
    die "not a directory: $background_dir"
    exit 1
fi

# random sort
backgrounds=( $(ls "$background_dir" | sort -R) )

s="$(xdpyinfo | grep dimensions: | awk '{print $2}')"

say "start slideshow, dir: $background_dir, interval (secs): $slideshow_seconds"

i=0
while true; do
    bg="$background_dir/${backgrounds[i]}"

    # skip non-image files, and directories
    if file --mime-type "$bg" | grep -v ': image/' > /dev/null; then
        continue
    fi

    say "new background from: $bg"

    # resize and fit screen, in order to make it easily usable from
    # lockscr.sh.
    convert \
        -resize x$(echo $s | cut -dx -f2) \
        -resize "$(echo $s | cut -dx -f1)x<" \
        -gravity center  -crop $s+0+0 +repage \
        "$bg" "$xbg"

    feh --no-fehbg --bg-center "$xbg"
    sleep $slideshow_seconds
    i=$(( i + 1 ))

    # wrap around
    if [ $i -eq ${#backgrounds[@]} ]; then
        i = 0
    fi
done
