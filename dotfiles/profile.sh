# profile -- Login shells.

_source (){
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
export MYFS="/igk"
export SYSTEM="$(uname)"
export MYSYSTEM="$MY/systems/$(hostname)"

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

if [ ! "$GK_NOENV" = "x" ];
then
    : ;
else
    ### Import shell settings:
    # set ENV to a file invoked each time sh is started for interactive use.
    ENV=$HOME/.$(basename $SHELL)rc; export ENV
    export FROMLOGINPROFILE=yes
    _source $ENV
    unset FROMLOGINPROFILE
fi

