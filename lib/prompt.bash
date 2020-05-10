# prompt.bash --- bash prompt

bold='\[\033[1m\]'
reset='\[\033[0m\]'

bp_lastcmdexit () {
    xit=$?
    [ $xit -ne 0 ] && printf "\033[7m\033[1m> $xit!\033[0m "
}

bp_queue () {
    if [ -d "$MAILQUEUE" ]; then
        count="$(( $(ls $MAILQUEUE | wc -l) / 2 ))"
        [ "0" != "$count" ] && echo "Queued messages: $count";
    fi
    true
}

bp_git_branch () {
    git rev-parse --abbrev-ref HEAD 2>/dev/null
}

bp_git_dirty () {
    test -n "$(git status -s 2>/dev/null)"
}

bp_hg_branch () {
    hg identify -b 2>/dev/null
}

bp_hg_dirty () {
    test -n "$(hg st 2>/dev/null)"
}

bp_branch () {
    w="$(bp_git_branch || bp_hg_branch)"
    d=""
    if bp_git_dirty || bp_hg_dirty; then
        d="#"
    fi
    if [ -n "$w" ]; then
        echo " on branch $w$d;"
    fi
}

bp_procmd () {
    # Should always be the first thing in this function so that it
    # is run just after the last evaluated command whose exit code
    # is in $?.
    bp_lastcmdexit
    line1='[In: \w; \d \A;$(bp_branch) ^$SHLVL]'
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
