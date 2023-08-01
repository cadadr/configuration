#!/usr/bin/env bash
# setbg.bash --- desktop background slideshow

. $MYLIB/fns.sh

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

pidfile="$HOME/.setbg.bash.pid"

if [ -e "$pidfile" ]; then
    ps -p "$(cat $pidfile)" > /dev/null && kill "$(cat $pidfile)"
fi

echo $$ > $pidfile

# Use SIGUSR1 to force next wallpaper, e.g.
#   pkill -USR1 -f setbg.bash
sleep_pid=
# We set it to -1 here so that when do_wlp initially starts, it sets
# it to idx++.  If we later catch a SIGUSR1, the while loop in do_wlp
# will be unconditionally terminated as per normal behaviour of Bash.
# In that case, assuming do_wlp will always add 1 to idx, we can just
# run do_wlp to restart the loop, it will increase the index by one.
# If we don’t do this hack, then do_wlp will always start with idx=0.
idx=-1
trap 'do_wlp' SIGUSR1
trap 'do_wlp_prev' SIGUSR2
trap 'clean_up_exit' INT TERM

slideshow_seconds="$(( ${GK_XBG_SLIDESHOW_INTERVAL_MINS-15} * 60 ))"

background_dir="${GK_XBG_SLIDESHOW_DIR-$HOME/.gk-xbg-dir}"

xbg="${GK_XBGIMG-$HOME/.xbg.png}"

if [ ! -d "$background_dir" ]; then
    die "not a directory: $background_dir"
    exit 1
fi

say "start slideshow, dir: $background_dir, interval (secs): $slideshow_seconds"

declare -a backgrounds

# Refresh backgrounds if the directory contents do not match the data
# we have loaded.
#
# Initially the backgrounds variable is empty, so will always load in that
# condition.
maybe_refresh_backgrounds(){
    readarray -t current_bgs < <(printf '%s\0' "${backgrounds[@]}" | sort -z | xargs -0n1)
    directory_contents=( $(ls "$background_dir" | sort) )

    if [ ! "${current_bgs[*]}" = "${directory_contents[*]}" ]; then
        backgrounds=( $(ls "$background_dir" | sort -R) )
    fi
}

# TODO(2022-04-15): if $background_dir changes during execution, the
# loop gets borked and script hogs CPU.
do_wlp(){
    # Always advance, initially idx=-1.  If we’re called again, it’s
    # because of a SIGUSR1, where we need idx++ anyways.
    idx=$(( idx + 1 ))

    # Clean up stray sleep(1).
    [[ $sleep_pid ]] && kill "$sleep_pid" || :

    maybe_refresh_backgrounds

    while true; do
        # wrap around
        if [ $idx -gt $(( ${#backgrounds[@]} - 1 )) ]; then
            idx=0
        elif [ $idx -eq -1 ]; then
            idx=$(( ${#backgrounds[@]} - 1 ))
        fi

        echo "[$(( idx + 1 ))/${#backgrounds[@]}]"
        bg="$background_dir/${backgrounds[$idx]}"

        # skip non-image files, and directories
        if file --mime-type "$bg" | grep -v ': image/' > /dev/null; then
            continue
        fi

        say "new background from: $bg [$(( idx + 1 ))/${#backgrounds[@]}]"

        convert "$bg" "$xbg"

        feh --no-fehbg --bg-fill "$xbg"

        sleep $slideshow_seconds & sleep_pid=$!
        wait $sleep_pid

        idx=$(( idx + 1 ))
    done
}

do_wlp_prev(){
    idx=$(( $idx - 2 ))
    do_wlp
}

clean_up_exit(){
    [[ $sleep_pid ]] && kill "$sleep_pid"
    rm "$pidfile"
    exit
}

do_wlp
