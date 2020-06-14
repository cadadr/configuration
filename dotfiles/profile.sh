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

### Locale and time zone:

. $MYLIB/loctz.sh

###

### Paths:

. $MYLIB/paths.sh

###

### Environment:

. $MYLIB/env.sh

###

### Guix:

. $MYLIB/guix/guix-env.sh

###

### Import shell settings:
# set ENV to a file invoked each time sh is started for interactive use.
ENV=$HOME/.$(basename $SHELL)rc; export ENV
export FROMLOGINPROFILE=yes
. $ENV
unset FROMLOGINPROFILE

