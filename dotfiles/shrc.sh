# UNIX Shell configuration.
#
# This is a mostly POSIX compatible shell RC file. Setup for
# compatible shells should be found under $MYLIB/rc.<shell-name>, e.g.
# $MYLIB/rc.bash.

export GK_SHELL=$(which $(echo $0 | sed 's/^-//'))

type source >/dev/null || source () {
        . $@
}

###

### Default programs:

EDITOR=NONE
if [ "x$INSIDE_EMACS" = x ]; then
    # Find a sensible editor.
    for EDITOR in vim vi zile nano ex ed; do
        if which $EDITOR >/dev/null 2>&1; then
	    export EDITOR;
	    [ $EDITOR = vim ] && alias vi=vim
	    break
        fi
    done
    PAGER=less
elif [ "x$SSH_CONNECTION" != x ] && [ "x$TERM" = xdumb ]; then
    EDITOR=ed
    PAGER=tee
else
    EDITOR=emacsclient
    PAGER=$HOME/.emacs.d/extras/eless.sh
fi


which $EDITOR 2>&1 >/dev/null || echo WARNING: no suitable editor found...

export EDITOR
export PAGER

###

### Environment:

export GPG_TTY=$(tty)

###

### Shell settings:

# Reset prompts
export PS1='$ '
export PS2='> '
export PS4='+ '

#### Load shell-specific stuff:
export GK_SHELL_RC="$MYLIB/rc.$(basename $GK_SHELL)"
test -e "$GK_SHELL_RC" && source "$GK_SHELL_RC"
alias vishrc="vi $GK_SHELL_RC"

###

#### Aliases & functions:
test -n "$MYLIB" && source $MYLIB/aliases.sh

# Rebuild known binary list.
hash -r

