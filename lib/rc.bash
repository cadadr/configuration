# rc.bash --- bash specific shell setup

shopt -s histappend
shopt -s checkwinsize
shopt -s globstar

# Show command line time information in a single line.
TIMEFORMAT=$'\n[time elapsed: real: %lR	user: %lU	sys: %lS]'
export TIMEFORMAT

source $MYLIB/prompt.bash;

# Dance for TRAMP!
#
# Tramp gets confused if the Bash prompt is multiline.  It connects to
# the remote shell over a dumb terminal, runs ssh, inserts a command,
# and starts interacting with the new shell, whose prompt it sets.  But
# if the very first prompt is multiline, it can't use it for some
# reason.
#
# So, this hack here disables the prompt command, i.e. the first line of
# the prompt, if this is a dumb terminal.  But Emacs' shell-mode uses
# dumb terminals too, and I do want my prompt there, so before disabling
# it, it checks if INSIDE_EMACS is defined.  Tramp does not define that
# variable for the ssh command, so if it is defined, this is not a tramp
# connection session, so the prompt command need not be disabled.
#
# Huh.
export PROMPT_COMMAND=bp_procmd

if [ "$TERM" = "dumb" ]; then
    if [ -z "${INSIDE_EMACS+x}" ]; then
	export PROMPT_COMMAND=''
    fi
fi

export HISTTIMEFORMAT='%s'
