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
    dpy="${1:-0}"
    startx -- :$dpy > $HOME/log/xsession:$dpy-stdout.log \
         2> $HOME/log/xsession:$dpy-error.log
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

para() {
    export LEDGER_FILE="$HOME/Documents/finans/$(date +'%Y-%m').dat"
    if [ -z "$@" ]; then
         hledger balance
    else
        case "$1" in
            var) hledger balance varlık ;;
            *) hledger $@ ;;
        esac
    fi
}

yw() {
    ydl -o- "$1" 2>/dev/null | mpv - 1>/dev/null 2>/dev/null
}

qr() {
    out="$(mktemp --tmpdir qrXXXXXXXXXXXXXX.png)"
    chmod 600 "$out"
    qrencode -l H -s 10 -o "$out" "$@"
    display "$out" &
    sleep 1
    rm "$out"
}

###
alias listall="alias | cut -d= -f1 && declare -F | cut -d ' '  -f 3 | sed 's,^,function ,'"
alias edit="$EDITOR"
alias re="refresh && repath"
alias j=jobs
# Group dirs first only for GNU ls.  For non-GNU, ‘ls --version’ will
# probably error, which should suffice for choosing the correct
# branch.
#
# The rest of ‘ls’ aliases will pick up these settings, so make sure
# the flags here are relevant for all interactive uses of ls(1).
if ls --version 2>/dev/null | grep -qs GNU; then
    alias ls='/bin/ls --group-directories-first -Fh'
else
    alias ls='/bin/ls -F'
fi
alias la='ls -Al'
alias lr='ls -lR'
alias mo=pg
YOUDL=$HOME/co/External/youtube-dl
alias ydl="PYTHONPATH=$YOUDL $YOUDL/bin/youtube-dl"
alias mpv="PYTHONPATH=$YOUDL mpv"
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
alias msgs="dmesg | tail"
alias msgrep="dmesg | grep"
alias mysudo="sudo --preserve-env=PATH" # https://news.ycombinator.com/item?id=18902265
alias umm="man -k"
# From: https://news.ycombinator.com/item?id=18909446
alias fs="mount | grep ^/ | column -t | sort"
# CD to the root of the current git project
# From: https://news.ycombinator.com/item?id=18910827
alias gup='git rev-parse --git-dir >/dev/null 2>&1 && cd `git rev-parse --show-toplevel` || echo "Not in git repo"'
# Python 3
alias venv='python3 -m venv'
alias mkvenv='venv .venv'
alias usevenv='( source ./.venv/bin/activate && bash -i )'
alias py='run-python.sh'
alias python='python3'
alias pip='pip3'
# Directory navigation
alias up='cd ..'
alias right='cd "$( find -L ${PWD%/*}/ -maxdepth 1 -type d -not -name \".*\" | sort | grep -A 1 ../${PWD##*/} | tail -n 1 )"'
alias left='cd "$( find -L ${PWD%/*}/ -maxdepth 1 -type d -not -name \".*\" | sort | grep -B 1 ../${PWD##*/} | head -n 1 )"'
# Node / Yarn
alias yarn=yarnpkg
# Docker
# Delete all containers that are not running:
# Adapted from https://linuxhint.com/cleanup-docker/
alias undocker='docker ps -a -f status=exited | sed "1d" | cut -d " " -f 1 | xargs docker rm'
# Suppress interactive progress reporting.
curl='curl -s'
# The backup command string
alias do-backup="borg create --stats --progress --compression lz4 ::{user}-{now} $MYFS"
alias vialiases="$EDITOR $MYLIB/aliases.sh"
alias envgrep="env | grep -i"
alias openpdfs="pgrep -af '\.pdf' | cut --complement -d ' ' -f1,2"
alias slrn="NNTPSERVER='news.gmane.io' HOME='$HOME/posta/news' slrn -i \
        '$MEINE/slrnrc'"
alias pdf2doc='soffice --infilter="writer_pdf_import" --convert-to doc'
alias pdf2odt='soffice --infilter="writer_pdf_import" --convert-to odt'


### OS specific aliases:

# Generic Linux aliases.
__gk_linux_aliases(){
    alias uctl='systemctl --user'
    alias hows='systemctl status'
    alias stop='systemctl stop'
    alias start='systemctl start'
    alias enable='systemctl enable'
    alias disable='systemctl disable'

    # Load distro-specific aliases.
    if [ -f /etc/os-release ]; then
        case $(grep '^ID=' /etc/os-release | cut -d= -f2) in
            debian|raspbian|ubuntu|linuxmint)
                __gk_debian_aliases ;;
        esac
    fi
}

# Aliases for Debian and descendants.
__gk_debian_has(){
    apt-cache search "$1" | grep "^$1\>"
}

__gk_debian_aliases(){
    alias listdeps="apt-cache depends --no-recommends --no-breaks --no-suggests\
                           --no-conflicts --no-enhances --no-replaces --recurse"
    alias fresh='sudo apt-get update && (apt list --upgradable | pg)'
    alias up='sudo apt-get upgrade'
    alias distup='sudo apt-get dist-upgrade'
    alias haz="dpkg-query -l"
    alias has=__gk_debian_has
    alias like="apt-cache search"
    alias bdeps="sudo apt-get build-dep"
    alias wanna="sudo apt-get install"
    alias eww="sudo apt-get autoremove"
    alias show="apt-cache show"
    alias whose="dpkg-query -S"
    alias what="dpkg-query -L"
}

# Aliases for FreeBSD systems
__gk_freebsd_aliases(){
    alias describe='pkg search -Q description'
    alias netrestart='sudo service netif restart ; sleep 3 ;\
                            sudo service routing restart'
}

case $SYSTEM in
    Linux) __gk_linux_aliases ;;
    FreeBSD) __gk_freebsd_aliases ;;
esac

