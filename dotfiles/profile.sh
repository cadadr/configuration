# profile -- Login shells.

GK_SOURCED=""

_source (){
    export GK_SOURCED="$1${GK_SOURCED:+:$GK_SOURCED}"
    case $0 in
	*zsh)	source $@ ;;
	*)	. $@ ;;
    esac
}

### Import system settings:
_source /etc/profile >/dev/null 2>&1 || echo Could not source /etc/profile...

###

### Variables and system information:

export MY="$HOME/cf"
export MEINE="$MY/dotfiles"
export MYLIB="$MY/lib"
export SYSTEM="$(uname)"
export MYSYSTEM="$MY/systems/$(hostname)"
export MYLOGS="${TMPDIR-/tmp}/$USER-logs"

# XXX: not a great idea to modify the filesystem in a profile script...
mkdir -p $MYLOGS
chmod 700 $MYLOGS

###

### Guix:

# guix.sh knows what to do when it's not on a Guix(SD) system, so no
# need for conditional inclusion here.
_source $MYLIB/profile/guix.sh

###


### Locale and time zone:

_source $MYLIB/profile/loctz.sh

###

### Paths:

_source $MYLIB/profile/paths.sh

###

### Environment:

_source $MYLIB/profile/env.sh

###

### XDG:

if [ "$(uname)" = "Linux" ]; then
    _source $MYLIB/profile/xdg.sh
fi

###


### Local files:

GK_PROFILE_EXTRA_DIR="$HOME/.config/profile.d"

if [ -d "$GK_PROFILE_EXTRA_DIR" ]; then
    for file in $(/bin/ls "$GK_PROFILE_EXTRA_DIR"); do
	_source "$GK_PROFILE_EXTRA_DIR/$file"
    done
fi

###

export ENV="$HOME/.shrc"

_source "$ENV"

