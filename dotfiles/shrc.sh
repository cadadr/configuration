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
    shopt -s autocd
    bold='\[\033[1m\]'
    reset='\[\033[0m\]'

    lastcmdexit () {
	xit=$?
        [ $xit -ne 0 ] && printf "\033[7m\033[1m> $xit!\033[0m "
    }
    PROMPT_COMMAND=lastcmdexit
    PS1="$bold"'[In: \w; \d \A]\n[\#] \u@\H (\j)\$'"$reset "
    HISTTIMEFORMAT='%s'
fi

###

#### Aliases & functions:
path () {
	echo $PATH | tr : \\n
}
repath () {
    . $MYLIB/paths.sh
}
refresh () {
    . $ENV
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
mkemacsbdir () {
    f=$(mktemp -d emacs-build.XXXXXX)
    git clone ~/co/External/emacs -b gk "$f" && echo "$f"
}

# This function uses pushd and popd to make a better cd.  It imitates
# cd, but maintains a full history of navigation.  Luckily, pushd and
# popd uses CDPATH too, so this can seamlessly replace cd.
c () {
    if [ x$1 = x- ]; then
	popd
    elif [ $# = 0 ]; then
	if [ ! $PWD = $HOME ]; then
	    pushd $HOME		# pushd w/ no args does not behave
				# like cd
	fi
    else
	pushd $@
    fi
}

countpdfpagesdir () {
    exiftool -T -filename -PageCount -s3 -ext pdf $1\
	| awk 'BEGIN{X=0}; {X+=$2}; END{print X}'
}

sx () {
    startx -- :${1:-0}
}

# From: https://news.ycombinator.com/item?id=18898898
pycd () {
    pushd $(python3 -c "import os.path, $1; print(os.path.dirname($1.__file__))");
}

# Combined find+grep, e.g. fgr "Class" "*.cpp" "-l" (2nd and 3rd
# parameters optional
# From: https://news.ycombinator.com/item?id=18909446
fgr () {
    NAM=""
    GREPTYPE="-i -H"
    if [ -n "$1" ]; then
        test -n "$2" && NAM="-name \"$2\""
        test -n "$3" && GREPTYPE=$3
        CMMD="find . $NAM -not -path '*/\.*' -exec egrep --colour=auto $GREPTYPE \"$1\" {} + 2>/dev/null"
        >&2 echo -e "Running: $CMMD\n"
        sh -c "$CMMD"
        echo ""
    else
        echo -e "Expected: fgr <search> [file filter] [grep opt]\n"
    fi
}

# Remount filesystems.
# usage: remount [MODE] [FS]
# MODE defaults to `ro' and FS defaults to `$PWD'.
remount () {
    FS="${2:-$PWD}"
    if [ "$FS" = "$PWD" ]; then
        cd /
    fi
    MODE="${1:-ro}"
    if [ "$MODE" = "-" ]; then
        MODE=ro
    fi
    sudo mount -o "remount,$MODE" "$FS"
    cd "$FS"
}

###
alias edit="$EDITOR"
alias re=". $ENV"
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
# Vagrant gets confused when GEM_* variables are set...
alias vagrant="env -u GEM_HOME -u GEM_PATH vagrant"
# Show file name and line numbers.
alias ngrep="grep -Hn"
alias msgs="sudo dmesg | tail"
alias sudo="sudo --preserve-env" # https://news.ycombinator.com/item?id=18902265
alias wanna="man -k"
# From: https://news.ycombinator.com/item?id=18909446
alias fs="mount | grep ^/ | column -t | sort"
# CD to the root of the current git project
# From: https://news.ycombinator.com/item?id=18910827
alias up='git rev-parse --git-dir >/dev/null 2>&1 && cd `git rev-parse --show-toplevel` || echo "Not in git repo"'
alias venv='python3 -m venv'

#### Debian:
alias deps='apt-cache depends --no-recommends --no-breaks --no-suggests --no-conflicts --no-enhances --no-replaces --recurse'
alias fresh='sudo apt-get update && (apt list --upgradable | pg)'
alias haz="dpkg-query -l"
alias has="apt-cache search"
alias show="apt-cache show"

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


