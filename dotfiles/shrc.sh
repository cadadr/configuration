# UNIX Shell configuration.
#
# This is a mostly POSIX compatible shell RC file, with GNU Bash
# specific stuff executed only if the $SHELL is bash.

export SHRC_VERSION=bobby
export SHELL

if [ x$FROMLOGINPROFILE = xyes ]; then
    echo Login profile script is loading shell setup...
fi

###

### Default programs:

EDITOR=NONE
if [ "x$INSIDE_EMACS" = x ]; then
    # Find a sensible editor.
    for EDITOR in vi vim zile nano ex ed; do
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

if which $EDITOR 2>&1 >/dev/null; then
    true
else
    echo WARNING: no suitable editor found...
fi

export EDITOR
export PAGER

###

### Environment:

export GPG_TTY=$(tty)

###

### Shell settings:

#### Bash-specific stuff:
#if [ x$0 = x-bash -o x$0 = xbash -o x$(basename x$0) = xbash ]; then
if [ ! x$BASH = x ]; then
    shopt -s histappend
    shopt -s checkwinsize
    shopt -s globstar

    lastcmdexit () {
	xit=$?
        [ $xit -ne 0 ] && echo "		*** It exited $xit! ***"
    }
    PROMPT_COMMAND=lastcmdexit
    PS1='[\#] \u@\H:\w (\j)\$ '
    HISTTIMEFORMAT='%s'
fi

###

#### Aliases & functions:
path () {
	echo $PATH | tr : \\n
}
whichroot () {
    if [ -z "${debian_chroot:-}" ] && [ -r /etc/debian_chroot ]; then
        cat /etc/debian_chroot
    else
	echo Niente chroot
	return 1
    fi
}
c () {
	arg=$1; shift
	case $arg in
	st)	cvs -nq up | sort;;
	sx)	cvs -q status | grep ^F | grep -v Up-to-date$ | sort;;
	diff)	cvs -q diff $*;;
	import) cvs -d $CVSROOT import $(basename $(pwd)) $(whoami) start;;
	*)	cvs -q $arg $*;;
	esac
}
f () {
	fg %$@
}
b () {
	bg %$@
}
wv () {
	xdg-open file://$PWD/$1
}
cloudbleed () {
    if (dig "$1" +short | grep -q cloudflare); then echo yes; else echo no; fi
}
serve () {
    python3 -m http.server --bind localhost --cgi 8000
}
gx () {
	arg=$1; shift
	case $arg in
	ls) guix system list-generations;;
	p) guix package $*;;
	in) guix package -i "$*";;
	s) guix package -s "$*";;
	w) guix package "--show=$*";;
	up) guix pull && guix install -u;;
	re) guix package -m $HOME/.config/GK/guix/guix-user.scm;;
	# When the configuration file is not on the same filesystem with the guix command, it causes troubles.  Probably because it interferes with mounting and unmounting of filesystems.
	reco) sudo cp $HOME/.config/GK/guix/guix-system.scm /config.scm; sudo guix system reconfigure /config.scm;;
	*) guix $arg $*;;
	esac
}

mkemacsbdir () {
    f=$(mktemp -d emacs-build.XXXXXX)
    git clone ~/co/External/emacs -b gk "$f" && echo "$f"
}

###

alias apt-dependencies='apt-cache depends --no-recommends --no-breaks --no-suggests --no-conflicts --no-enhances --no-replaces --recurse'
alias j=jobs
alias ls='ls -F'
alias mo=pg
YOUDL=$HOME/co/External/youtube-dl
alias ydl="PYTHONPATH=$YOUDL $YOUDL/bin/youtube-dl"
alias pg="$PAGER"
alias o=xdg-open
# Recursively download a website at a given url.
alias wgetall='wget -r -p -E -k -np -w 1'
alias stracemacs='strace -p $(pgrep emacs) 2>~/log/stracemacs.log 1>~/log/stracemacs.log'
alias free='free -h'
# see https://www.mercurial-scm.org/wiki/MqTutorial#Versioning_our_patch_set
alias mq='hg -R $(hg root)/.hg/patches'

###

##### System specific aliases:
case $SYSTEM in
    Linux)
	alias uctl='systemctl --user'
	;;
    FreeBSD)
	alias describe='pkg search -Q description'
	alias netrestart='sudo service netif restart ; sleep 3 ; sudo service routing restart'
	;;
esac

###

# Rebuild known binary list.
hash -r


