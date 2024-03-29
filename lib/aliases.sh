# aliases.sh --- aliases and utility functions for interactive shell

path () {
	echo $PATH | tr : \\n
}

repath () {
    . $MYLIB/profile/paths.sh
    echo "repath: loaded $MYLIB/profile/paths.sh"
}

# remove from path
unpath () {
	export PATH="$(path | grep -v $@ | tr '\n' ':' | sed s,:\$,,)"
}

refresh () {
    if [ -f "$ENV" ]; then
	. $ENV
	echo "refresh: loaded $ENV"
    else
	echo 'refresh: error: $ENV not set, or file not found' >&2
	return 2
    fi
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
    if [ -z "$@" ]; then
        fg
    else
	fg %$@
    fi
}

b () {
    if [ -z "$@" ]; then
        bg
    else
	bg %$@
    fi
}

d () {
    if [ -z "$@" ]; then
        disown
    else
	disown %$@
    fi
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

# options:
#   -c: colourscheme, passed to $MYSYSTEM/desktop-setup.bash
#          default: light
#   -d: X display number to use, without the colon
#          default: 0    (zero)
sx () {
    dpy=0
    colourscheme=light

    while getopts "c:d:" opt; do
        case "${opt}" in
            c) colourscheme="${OPTARG}" ;;
            d) dpy="${OPTARG}" ;;
        esac
    done
    shift $((OPTIND-1))

    log_stdout="$MYLOGS/xsession:$dpy-stdout.log"
    log_stderr="$MYLOGS/xsession:$dpy-error.log"
    log_xorg="$HOME/.local/share/xorg/Xorg.$dpy.log"

    # Preserve old logs
    [ -e $log_stdout ] && cp $log_stdout $log_stdout.old
    [ -e $log_stderr ] && cp $log_stderr $log_stderr.old

    case "$colourscheme" in
        dark)
            export GK_COLOUR_SCHEME_PREFERENCE=dark
            startx -- :$dpy > $log_stdout 2> $log_stderr
            ;;
        light)
            export GK_COLOUR_SCHEME_PREFERENCE=light
            startx -- :$dpy > $log_stdout 2> $log_stderr
            ;;
        *)
            echo "invalid colourscheme: $colourscheme"
            return 2
    esac
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

# Use pandoc to convert org to docx
# Adapted from
# http://blog.lujun9972.win/emacs-document/blog/2020/02/19/convert-.org-to-.docx-with-citations/index.html
#
# Needs working tho
org2docxWIP() {
        pandoc \
            --bibliography=$1 \
            --csl=/home/g/co/External/github-citation-style-language-styles$2.csl \
            --reference-docx=$3 \
            -i $4 -o $4-pandoc.docx
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

hist() {
    history | awk 'match($0,/\s+([0-9]+)\s+([0-9]{,10})(.+)/,m) {
        "date -d @" m[2] | getline date;
        printf "\033[1m" date "\n\033[0m" m[3] "\n"
    }'
}

pdf_monofy() {
    [ "$1" = "-h" ] || [ -z "$1" ] \
        && echo "usage: pdf_monofy INPUT [DENSITY] [SUFFIX]" \
        && echo "defaults:                300       _monofied" \
        && return 1
    # better dumb than sorry:
    echo $1 | grep ".pdf$" >/dev/null || { echo "not a PDF file: $1" && return 1; }
    outnam="${1/.pdf}${3-_monofied}.pdf"
    convert -monochrome -density ${2-300} "$1" "$outnam"  2>&1 | \
        (
            xit=$?
            read output
            echo $output | grep "operation not allowed by the security policy \`PDF'" 1>/dev/null 2>/dev/null
            if [ "$?" -ne 1 ]; then
                echo "ImageMagick can't operate on PDF files probably because"
                echo "your version of it is affected by CVE-2020-29599 and your"
                echo "distribution has disabled the PDF coder.  If you so wish,"
                echo "you can enable it temporarily by modifying the file:"
                echo "    " /etc/ImageMagick-*/policy.xml
                echo "Specifically, find a line that looks like"
                echo '    <policy domain="coder" rights="none" pattern="PDF" />'
                echo "and comment it out."
                echo "For more info, viz. https://security-tracker.debian.org/tracker/CVE-2020-29599."
                return 3
            else
                print $output >&2
                [ "$xit" = "0" ] && echo "Wrote $outnam"
                return $xit
            fi
        )
}

_which(){
    case $GK_SHELL in
	*zsh)	which -p $@ ;;
	*)	which $@ ;;
    esac
}

gifify(){
    if [ $# -ne 2 ]; then
	echo "usage: gifify INFILE OUTFILE" ;
	return 2
    fi
    ffmpeg -i "$1" -f gif -filter_complex "[0:v] fps=5,scale=w=480:h=-1,split [a][b];[a] palettegen [p];[b][p] paletteuse" "$2"
}

texwut(){
    texdoc $@ 2>/dev/null 1>/dev/null &
}

gmi(){
    gemini_dir=$HOME/co/cadadr.space/
    if [ $# -lt 1 ]; then
	echo 'gmi: usage: gmi CMD'
	return 2
    fi
    case $1 in
	# New gemlog entry
	newlog|nl|l)
	    if [ $# -lt 3 ]; then
		echo "gem: usage: gem newlog GEMLOG SHORT_TITLE"
		return 2
	    fi
	    if [ ! -e "$gemini_dir/templates/$2.gmi" ]; then
		echo "template '$2' not found"
		return 3
	    fi
	    new="$gemini_dir/gemini/$2-$(date +%F)-$3.gmi"
	    sed "s/@@DATE@@/$(LANG=en date +'%B %d, %Y')/"  \
		< $gemini_dir/templates/$2.gmi	    \
		> $new
	    vi $new ;;
	cd)
	    cd $gemini_dir ;;
	*)
	    echo "gem: unknown command: $1"
	    return 2 ;;
    esac
}

# simple front-end to run a live iso image with qemu.
# usage: qemulive [cdrom-file] [hda-file]
qemulive() {
    qemu-system-x86_64 -boot order=d -m 4096 \
	-enable-kvm -smp 2 -cpu host \
	-net user,hostfwd=tcp::10022-:22 \
	-net nic \
	${1:+-cdrom "$1"} ${2:+-hda "$2"}
}

# Convert WebP images to PNG, because fuck WEBP
fuck_webp() {
    find . -iname '*.webp' -exec echo convert \{\} \
	\$\(basename -s .webp \{\}\).png \; | sh
    find . -iname '*.webp' -exec rm \{\} \+
}

# Clear recently used files information
clearrecents(){
    mkdir -p $HOME/.local/share # ensure the directory exists
    echo -n '' > $HOME/.local/share/recently-used.xbel
}

recentlogs(){
    pager="$PAGER"
    # If the pager is less(1), use the «raw control chars» option, in
    # order for the colour output to work. My Emacs pager shim ignores
    # the argument. more(1) doesn’t seem to have an argument lie that,
    # but doesn’t seem to require one either, it just works with the
    # generated output.
    if [ "$pager" = "less" ]; then
        pager="$pager -R"
    fi
    (
	echo "$(tput smul)Log files modified in the last three days$(tput sgr0)"
	echo
	find $MYLOGS/ -mindepth 1 -mtime -3 -not -name '*.old' -exec tail \{\} \+ \
	    | sed -E "s/^==> (.*)$/$(tput setaf 2)$(tput smul)==> \1$(tput sgr0)/" \
    ) | $pager
}

pvdtar() {
    if [ $# -ne 2 ]; then
	echo usage: pvtar SOURCE DEST
	return 1
    fi
    in=$1
    out=$2
    if [ ! -d $in ]; then
	echo pvtar: SOURCE should be a directory
	return 1
    fi
    if [ -e $out ]; then
	echo pvtar: something exists at DEST, will not override or modify
	return 1
    fi

    # adapted from pv(1) man page
    (
	tar cf - $in \
	    | pv -n -s $(du -sb $in | awk '{print $1}') \
	    | gzip -9 > $out
    ) \
	2>&1 \
	| dialog --gauge \
	    "source: $in, destination: $out. progress:"\
	    0 $(( COLUMNS - 10 ))
}

###
# ssh errors with `unknown terminal' without this, see
# https://sw.kovidgoyal.net/kitty/faq/#i-get-errors-about-the-terminal-being-unknown-or-opening-the-terminal-failing-when-sshing-into-a-different-computer
if [ "xterm-kitty" = "$TERM" ]; then
    alias ssh="kitty +kitten ssh"
fi
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
    alias ls="$(_which ls) --group-directories-first -Fh"
else
    alias ls="$(_which ls) -F"
fi
alias la='ls -Al'
alias lr='ls -lR'
alias mo=pg
if which yt-dlp >/dev/null 2>/dev/null; then
    alias ydl=yt-dlp
    alias mpv="$(which mpv)"
    alias ydlmusic="ydl --extract-audio --audio-format vorbis --audio-quality 3 --no-progress --batch-file -"
else
    YOUDL=$HOME/co/External/youtube-dl
    alias ydl="PYTHONPATH=$YOUDL python3 $YOUDL/bin/youtube-dl"
    alias mpv="PYTHONPATH=$YOUDL mpv"
    alias ydlup="(cd $YOUDL && git pull)"
    alias ydlmusic="ydlup && ydl --extract-audio --audio-format vorbis --audio-quality 3 --no-progress --batch-file -"
fi
# Medium quality
alias ydl720="ydl -f 'bestvideo[height<=720]+bestaudio/best[height<=720]'"
alias ydl480="ydl -f 'bestvideo[height<=480]+bestaudio/best[height<=480]'"
alias ydl360="ydl -f 'bestvideo[height<=360]+bestaudio/best[height<=360]'"
alias pg="$PAGER"
alias o=xdg-open
# Recursively download a website at a given url.
alias wgetall='wget -r -p -E -k -np -w 1'
alias stracemacs='strace -p $(pgrep emacs) 2>/tmp/stracemacs.log 1>/tmp/stracemacs.log'
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
alias sudoe="sudo -E"
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
alias vi="$EDITOR"
alias vialiases="$EDITOR $MYLIB/aliases.sh"
alias videsktop="$EDITOR $MYSYSTEM/desktop-setup.bash"
alias vi3wm="$EDITOR $HOME/.config/i3/config"
alias vimrc="vi ~/.vimrc"
alias mysys="cd $MYSYSTEM"
alias envgrep="env | grep -i"
alias openpdfs="pgrep -af '\.pdf' | cut --complement -d ' ' -f1,2"
alias slrn="NNTPSERVER='news.gmane.io' HOME='$HOME/posta/news' slrn -i \
        '$MEINE/slrnrc'"
alias pdf2doc='soffice --infilter="writer_pdf_import" --convert-to doc'
alias pdf2odt='soffice --infilter="writer_pdf_import" --convert-to odt'
alias du="du -h"
alias df="df -h"
alias tmux='TERM=screen-256color-bce tmux'
alias etcup="( cd $MYSYSTEM && sudo bash $MYLIB/install-configs.bash )"
pwd_to_emacspath(){
    emacsclient -eval "(add-to-list 'load-path \"$PWD\")"
}
alias pwd2emacspath=pwd_to_emacspath
alias lofi="mpv --volume=30 --no-video 'https://www.youtube.com/watch?v=5qap5aO4i9A'"
alias pgrep="pgrep -fa"
alias crup="crontab $MY/cron/$(hostname).crontab"
alias vicron="vi $MY/cron/$(hostname).crontab && crup && crontab -l"
alias cronlog="sudo grep CRON /var/log/syslog"
# these count / show characters from the pipe that are outside the ASCII 7bit chars.
alias notascii="tr -d '[:alnum:][:punct:][:space:]' | wc -m"
alias shownotascii="tr -d '[:alnum:][:punct:][:space:]' | sed -E 's/(.)/\1\n/g' | sort | uniq -c | column"
alias ohfuckemacs="pkill -USR2 emacs"
alias localbogie="BOGIE_PLAYLIST=~/trains/ BOGIE_RADIO=~/Music/study-instrumental/ bogie.bash"
alias partlocalbogie="BOGIE_PLAYLIST=~/trains/ bogie.bash"
alias q=exit
# From: https://gist.github.com/mkgin/6763914a9f1a54ad374898e4f9e51a48
#
# du -hs for both dotfiles & normal files. normally "du -hs ." just shows stats
# for `.' & `..'`
alias du_all='du -sch .[!.]* * | sort -h'
alias mimels='file -bi * | sort | uniq -c'

# raspberry pi
alias pi="ssh pi@xanthippe.local"
alias pi_temp="ssh pi@xanthippe.local vcgencmd measure_temp"

### OS specific aliases:

# Generic Linux aliases.
__gk_linux_aliases(){
    alias systemctl='systemctl -l'
    alias uctl='systemctl --user'
    alias hows='systemctl status'
    alias stop='systemctl stop'
    alias start='systemctl start'
    alias enable='systemctl enable'
    alias disable='systemctl disable'

    # Load distro-specific aliases.
    if [ -f /etc/os-release ]; then
        case $(grep '^ID=' /etc/os-release | cut -d= -f2) in
            debian|raspbian|ubuntu|linuxmint|neon)
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
    alias haz="PAGER=tee dpkg-query -l"
    alias has=__gk_debian_has
    alias like="apt-cache search"
    alias bdeps="sudo apt-get build-dep"
    alias wanna="sudo apt-get install"
    alias eww="sudo apt-get autoremove"
    alias show="apt-cache show"
    alias whose="dpkg-query -S"
    alias what="dpkg-query -L"
    alias lspkg="dpkg-query -L"
}

# Aliases for FreeBSD systems
__gk_freebsd_aliases(){
    alias describe='pkg search -Q description'
    alias netrestart='sudo service netif restart ; sleep 3 ;\
                            sudo service routing restart'
}

# Guix(SD) shortcut commands
gx(){
    case $1 in
        rs) echo Reconfiguring system: $MYSYSTEM/system.scm ;
            echo This action requires superuser privileges. ;
            sudo guix system reconfigure $MYSYSTEM/system.scm ;;

        ru) echo Upgrading profile from manifest: $MYSYSTEM/user.scm ;
            guix package --manifest=$MYSYSTEM/user.scm ;;

        su|both)
            echo Reconfigure system \($MYSYSTEM/system.scm\) and upgrade ;
            echo user profile \($MYSYSTEM/user\). ;
            echo The first step of this action requires superuser privileges. ;
            sudo guix system reconfigure $MYSYSTEM/system.scm && \
                 guix package --manifest=$MYSYSTEM/user.scm ;;

        up) echo Upgrading system: $MYSYSTEM/system.scm ;
            echo This action requires root privileges. ;
            sudo bash -c "guix pull && guix package -u && guix system reconfigure $MYSYSTEM/system.scm" ;;

        p) shift; guix package $@ ;;
        s) shift; guix package -s $@ ;;
        i) shift; guix package --show=$@ ;;
        I) shift; guix package -I $@ ;;

        vu) edit $MYSYSTEM/user.scm ;;
        vs) edit $MYSYSTEM/system.scm ;;

        h|help|-h|--help|-help|'')
           echo 'usage: gx COMMAND [OPTIONS]' ;
           echo ;
           echo "Göktuğ's Guix(SD) Shortcuts" ;
           echo ;
           echo Commands:
           printf "\trs\treconfigure system*\n" ;
           printf "\tru\tupdate user's profile from manifest\n" ;
           printf "\tup\trun 'guix pull' then upgrade system\n" ;
           printf "\tsu\treconfigure system then update user profile\n" ;
           printf "\t\tfrom manifest (alias: both)\n" ;
           echo
           printf "\tp\talias for 'guix package'\n" ;
           printf "\ts\talias for 'guix package -s' (search)\n" ;
           printf "\ti\talias for 'guix package --show=' (package info)\n" ;
           printf "\tI\talias for 'guix package -I' (check installed)\n" ;
           echo ;
           printf "\tvu\tedit user configuration ($MYSYSTEM/user.scm)\n" ;
           printf "\tvs\tedit system configuration ($MYSYSTEM/system.scm)\n" ;
           echo
           printf "\th\tshow this help message\n" ;
           echo ;
           echo "* requires superuser privileges" ;;


        *) echo unknown subcommand: $@ ; gx h ;;
    esac
}

case $SYSTEM in
    Linux) __gk_linux_aliases ;;
    FreeBSD) __gk_freebsd_aliases ;;
esac

