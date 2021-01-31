# profile -- Login shells.

### Import system settings:
# . /etc/profile >/dev/null 2>&1 || echo Could not source /etc/profile...

###

### Variables and system information:

export MY="$HOME/cf"
export MEINE="$MY/dotfiles"
export MYLIB="$MY/lib"
export MYFS="/igk"
export SYSTEM=$(uname)

###

### Guix:

# guix.sh nows what to do when it's not on a Guix(SD) system,
# so no need for conditional inclusion here.
. $MYLIB/profile/guix.sh

###


### Locale and time zone:

. $MYLIB/profile/loctz.sh

###

### Paths:

. $MYLIB/profile/paths.sh

###

### Environment:

. $MYLIB/profile/env.sh

###

### Import shell settings:
# set ENV to a file invoked each time sh is started for interactive use.
ENV=$HOME/.$(basename $SHELL)rc; export ENV
export FROMLOGINPROFILE=yes
. $ENV
unset FROMLOGINPROFILE

