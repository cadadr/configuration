# prompt.bash --- bash prompt

bp_lastcmdexit () {
    xit=$?
    [ $xit -ne 0 ] && printf \
	"$(tput setab 9)$(tput setaf 16) $xit!$(tput sgr0) "
}

bp_queue () {
    if [ -d "$MAILQUEUE" ]; then
        count="$(( $(ls $MAILQUEUE | wc -l) / 2 ))"
        [ "0" != "$count" ] && echo " <✉:$count> ";
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
#
# Output the identifier of the VCS tool if found.
bp_is_repo () (
    while [ ! "$PWD" = '/' ]; do
	if [ -d .git ]; then
	    echo git
	    exit 0
	elif [ -d .hg ]; then
	    echo hg
	    exit 0
	fi
	cd ..
    done
    exit 1
)

bp_branch () {
    maybe_repo=$(bp_is_repo)
    if [ -n "$maybe_repo" ]; then
	branch=$(bp_${maybe_repo}_branch)
	dirty=''
	if bp_${maybe_repo}_dirty; then
	    dirty="#"
	fi
	if [ -n "$branch" ]; then
	    echo "<$(tput smul)$branch$dirty$(tput rmul)>"
	fi
    fi
}

bp_venv () {
    # If a python virtual environment is active, provide relevant
    # info.
    if [ -n "$VIRTUAL_ENV" ]; then
	venvname="$(realpath --relative-to=$PWD $VIRTUAL_ENV)"
	venvpyver="v$($venvname/bin/python --version | cut -d ' ' -f 2)"
	echo " <venv:$venvname@$venvpyver> "
    fi
}

bp_guix () {
    # If a Guix environment is active, provide relevant info.
    if [ -n "$GUIX_ENVIRONMENT" ]; then
        echo " <guix:$GUIX_ENVIRONMENT> "
    fi
}

bp_termwidth () {
    tput cols
}

bp_prompt_long_p () {
    ps1_expanded="${1@P}"
    ps1_expanded="$(echo -ne $ps1_expanded | sed $'s/\e\\[[0-9;:]*[a-zA-Z]//g')"
    [ ${#ps1_expanded} -gt $(( $(bp_termwidth) / 2 )) ]
    return $?
}

bp_procmd () {
    # Should always be the first thing in this function so that it
    # is run just after the last evaluated command whose exit code
    # is in $?.
    bp_lastcmdexit

    bold="\[$(tput bold)\]"
    reset="\[$(tput sgr0)\]"

    # if xterm, colour
    case $TERM in
	xterm*|alacritty)
	    color="$(seq 1 16 | grep -ve '[789]' -e '1' -e '16' | shuf -n 1)"
	    bold="\[$(tput setaf $color)\]"
	    ;;

	* ) # Do nothing.
	    ;;
    esac

    PS1="$bold"'#'"\#$reset \D{%F %H:%M} "
    PS1+="$(bp_venv)$(bp_guix)$(bp_queue)$bold\u@\H$reset:\w"
    PS1+="$(bp_branch)"

    if bp_prompt_long_p "$PS1"; then
	PS1+='\n'
    fi

    PS1+="$bold(\jj/^$SHLVL)\$$reset "

    unset bold reset ps1_expanded
}
