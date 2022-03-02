# prompt.bash --- bash prompt

bp_lastcmdexit () {
    xit=$?
    echo
    [ $xit -ne 0 ] && printf \
	"$(tput setab 9)$(tput setaf 16) $xit!$(tput sgr0) "
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

# Determine if we're within a supported VCS repo or not.  Faster than
# just trying to run all `bp_*_branch' commads.
bp_is_repo () (
    while [ ! "$PWD" = '/' ]; do
	if [ -d .git ] || [ -d .hg ]; then
	    exit 0
	fi
	cd ..
    done
    exit 1
)

bp_branch () {
    bp_is_repo || return
    w="$(bp_git_branch || bp_hg_branch)"
    d=""
    if bp_git_dirty || bp_hg_dirty; then
        d="#"
    fi
    if [ -n "$w" ]; then
	echo " on branch $(tput smul)$w$d$(tput rmul);"
    fi
}

bp_procmd () {
    # Should always be the first thing in this function so that it
    # is run just after the last evaluated command whose exit code
    # is in $?.
    bp_lastcmdexit

    bold="$(tput bold)"
    reset="$(tput sgr0)"

    # if xterm, colour
    case $TERM in
	xterm-*)
	    color="$(seq 1 16 | grep -ve '[789]' -e '1' -e '16' | shuf -n 1)"
	    bold="$(tput setaf $color)"
	    ;;
	* ) ;;
    esac

    line1='[In: \w; \d \A;$(bp_branch) ^$SHLVL]'
    line2='[\#] \u@\H (\j)\$'
    # A python virtual environment is active, provide relevant
    # info.
    if [ -n "$VIRTUAL_ENV" ]; then
        venvname="$(realpath --relative-to=$PWD $VIRTUAL_ENV)"
        venvpyver="v$($venvname/bin/python --version | cut -d ' ' -f 2)"
        PS1="\[$bold\]$line1\[$reset\] (venv:$venvname@$venvpyver)\n\[$bold\]$line2\[$reset\] "
    # A Guix environment is active, provide relevant info.
    elif [ -n "$GUIX_ENVIRONMENT" ]; then
        PS1="\[$bold\]$line1\[$reset\] (guix:$GUIX_ENVIRONMENT)\n\[$bold\]$line2\[$reset\] "
    else
        PS1="\[$bold\]$line1\n$line2\[$reset\] "
    fi
    bp_queue
}
