# prompt.bash --- bash prompt

bold='\[\033[1m\]'
reset='\[\033[0m\]'

bp_lastcmdexit () {
    xit=$?
    [ $xit -ne 0 ] && printf "\033[7m\033[1m> $xit!\033[0m "
}

bp_queue () {
    count="$(( $(ls $MAILQUEUE | wc -l) / 2 ))"
    [ "0" != "$count" ] && echo "Queued messages: $count";
    true
}

bp_procmd () {
    # Should always be the first thing in this function so that it
    # is run just after the last evaluated command whose exit code
    # is in $?.
    bp_lastcmdexit
    line1='[In: \w; \d \A; ^$SHLVL]'
    line2='[\#] \u@\H (\j)\$'
    # A python virtual environment is active, provide relevant
    # info.
    if [ -n "$VIRTUAL_ENV" ]; then
        venvname="$(realpath --relative-to=$PWD $VIRTUAL_ENV)"
        venvpyver="v$($venvname/bin/python --version | cut -d ' ' -f 2)"
        PS1="$bold$line1$reset ($venvname: $venvpyver)\n$bold$line2$reset "
    else
        PS1="$bold$line1\n$line2$reset "
    fi
    bp_queue
}
