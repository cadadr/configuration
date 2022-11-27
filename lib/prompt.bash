# prompt.bash --- bash prompt

_bp_histcmd="$HISTCMD"

# Parameter to set the prompt style on the fly.
BP_PROMPT_STYLE=simple

bp_lastcmdexit () {
    local last_cmd_exit_code=$?
    # Check if history index advanced, and only print anything if that
    # happened. When user hits Return in an empty prompt, or when a command
    # line is cancelled with ^C, the history index is not updated, it stays
    # the same number as before the submission/cancellation of that command
    # line. We exploit this to avoid printing the error message for the same
    # command multiple times should some empty prompts be submitted, or
    # a command line be cancelled.
    #
    # XXX: does not work when the submitted command is a comment: the value
    # of \# in the prompt stays the same, but apparently HISTCMD is
    # incremented.
    if [ "$_bp_histcmd" -ne "$HISTCMD" ]; then
	local msg="command line failed with exit code"
	# XXX(2022-11-27): this clobbers the prompt if it's on the same line with it,
	# so i break the line after it and made it a bit longer just to make it
	# look nicer as such.
	[ $last_cmd_exit_code -ne 0 ] && \
	    printf "$(tput setab 9)$(tput setaf 16)$msg: $last_cmd_exit_code$(tput sgr0) \n"
    fi
    _bp_histcmd="$HISTCMD"
}

bp_queue () {
    if [ -d "$MAILQUEUE" ]; then
        local count="$(( $(ls $MAILQUEUE | wc -l) / 2 ))"
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
    local maybe_repo=$(bp_is_repo)
    if [ -n "$maybe_repo" ]; then
	local branch=$(bp_${maybe_repo}_branch)
	local dirty=''
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
	local venvname="$(realpath --relative-to=$PWD $VIRTUAL_ENV)"
	local venvpyver="v$($venvname/bin/python --version | cut -d ' ' -f 2)"
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
    local ps1_expanded="${1@P}"
    local ps1_expanded="$(echo -ne $ps1_expanded | sed $'s/\e\\[[0-9;:]*[a-zA-Z]//g')"
    [ ${#ps1_expanded} -gt $(( $(bp_termwidth) / 2 )) ]
    return $?
}

bp_colour_or_bold () {
    case $TERM in
	# if xterm, colour
	xterm*|alacritty)
	    local color="$(seq 1 16 | grep -ve '[789]' -e '1' -e '16' | shuf -n 1)"
	    printf "\[$(tput setaf $color)\]"
	    ;;

	# Otherwise, just bold face
	* ) printf "\[$(tput bold)\]"
	    ;;
    esac
}

# Info dense prompt that automatically breaks up into two lines if
# it's too wide.
bp_adaptive_prompt () {
    local bold="$(bp_colour_or_bold)"
    local reset="\[$(tput sgr0)\]"

    PS1="$bold"'#'"\#$reset \D{%F %H:%M} "
    PS1+="$(bp_venv)$(bp_guix)$(bp_queue)$bold\u@\H$reset:\w"
    PS1+="$(bp_branch)"

    if bp_prompt_long_p "$PS1"; then
	PS1+='\n'
    fi

    PS1+="$bold(\jj/^$SHLVL)\$$reset "
}

# Simplistic prompt with history index and job control info & shlvl
bp_simple_prompt () {
    local bold="$(bp_colour_or_bold)"
    local reset="\[$(tput sgr0)\]"

    PS1="$bold[\#: \jj/^$SHLVL] \$$reset "
}

bp_procmd () {
    # Should always be the first thing in this function so that it
    # is run just after the last evaluated command whose exit code
    # is in $?.
    bp_lastcmdexit

    bp_${BP_PROMPT_STYLE}_prompt
}
