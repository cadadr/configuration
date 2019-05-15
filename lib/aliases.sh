# aliases.sh --- aliases and utility functions for interactive shell

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

# Use pandoc to convert epub to pdf
epub2pdf () {
    case $# in
        2) pandoc -f epub -t latex -o $1.pdf --pdf-engine=xelatex $2 ;;
        *) echo "usage: epub2pdf OUTPUT-SANS-EXT INPUT-FILE"
    esac
}

###
alias listall="(alias | cut -d= -f1 && declare -F | cut -d ' '  -f 3 | sed 's,^,function ,')"
Listall(){ listall | pg; }
alias edit="$EDITOR"
alias re=". $ENV"
alias j=jobs
alias ls='ls -F'
alias la='ls -FAl'
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
Ngrep(){ ngrep $@ | pg; }
alias msgs="dmesg | tail"
Msgs(){ msgs $@ | pg ; }
alias msgrep="dmesg | grep"
Msgrep(){ msgrep $@ | pg; }
alias sudo="sudo --preserve-env" # https://news.ycombinator.com/item?id=18902265
alias umm="man -k"
Umm(){ umm $@ | pg; }
# From: https://news.ycombinator.com/item?id=18909446
alias fs="mount | grep ^/ | column -t | sort"
Fs(){ fs $@ | pg; }
# CD to the root of the current git project
# From: https://news.ycombinator.com/item?id=18910827
alias gup='git rev-parse --git-dir >/dev/null 2>&1 && cd `git rev-parse --show-toplevel` || echo "Not in git repo"'
# Python 3
alias venv='python3 -m venv'
alias mkvenv='venv .venv'
alias usevenv='( source ./.venv/bin/activate && bash -i )'
alias py='python3'
# Directory navigation
alias up='cd ..'
alias right='cd "$( find -L ${PWD%/*}/ -maxdepth 1 -type d -not -name \".*\" | sort | grep -A 1 ../${PWD##*/} | tail -n 1 )"'
alias left='cd "$( find -L ${PWD%/*}/ -maxdepth 1 -type d -not -name \".*\" | sort | grep -B 1 ../${PWD##*/} | head -n 1 )"'
# Mail
alias runq=msmtp-runqueue.sh
alias rq=msmtp-runqueue.sh
alias lisq=msmtp-listqueue.sh
alias lq=msmtp-listqueue.sh
# Node / Yarn
alias yarn=yarnpkg
# Docker
# Delete all containers that are not running:
# Adapted from https://linuxhint.com/cleanup-docker/
alias undocker='docker ps -a -f status=exited | sed "1d" | cut -d " " -f 1 | xargs docker rm'

### OS specific aliases:
case $SYSTEM in
    Linux)
        if [ -f /etc/os-release ]; then
            case $(grep '^ID=' /etc/os-release | cut -d= -f2) in
                debian|raspbian)
                    alias deps="apt-cache depends --no-recommends --no-breaks --no-suggests --no-conflicts --no-enhances --no-replaces --recurse"
                    Deps(){ deps $@ | pg; }
                    alias fresh='sudo apt-get update && (apt list --upgradable | pg)'
                    alias distup='sudo apt-get dist-upgrade'
                    alias haz="dpkg-query -l"
                    Haz(){ haz $@ | pg; }
                    alias has="apt-cache search"
                    Has(){ has $@ | pg; }
                    alias wanna="sudo apt-get install"
                    alias eww="sudo apt-get autoremove"
                    alias show="apt-cache show"
                    Show(){ show $@ | pg; }
                    alias whose="dpkg-query -S"
                    alias what="dpkg-query -L"
                    ;;
            esac
        fi
        alias uctl='systemctl --user'
        alias hows='systemctl status'
        Hows(){ hows $@ | pg; }
        alias stop='systemctl stop'
        alias start='systemctl start'
        alias enable='systemctl enable'
        alias disable='systemctl disable'
        ;;
    FreeBSD)
        alias describe='pkg search -Q description'
        alias netrestart='sudo service netif restart ; sleep 3 ; sudo service routing restart'
        ;;
esac

