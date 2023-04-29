# env.sh --- Shell environment.

if [ "$GK_PATHS_LOADED" != "yes" ]; then
    echo "$MYLIB/paths.sh should be loaded before $MYLIB/env.sh"
    exit 1
fi

export EMAIL=$USER@localhost
export MAILDIR=$HOME/Posta
export MAILQUEUE=$MAILDIR/queue
export PS1='\$ '
export PASSWORD_STORE_DIR=$HOME/Documents/Passwords
export TROFFONTS=$HOME/.fonts

# This made me spend months trying to download the CVS repo of
# OpenBSD...
export CVS_RSH=ssh

# Email
export NOTMUCH_CONFIG=$MEINE/mail/notmuch.ini

# Quilt
export QUILT_DIFF_ARGS="--color=never"	# Fuck colours. fuck colours.
export QUILT_PATCHES_ARGS="--color=never"
export QUILT_PUSH_ARGS="--color=never"
export QUILT_SERIES_ARGS="--color=never -v"
export QUILT_PAGER="tee"	# Fuck pagination.  I know how to pipe to less.

# Spamd
export SPAMD_PORT=2338
export SPAMD_PID=$HOME/var/spamd.pid
export SPAMD_LOG=$HOME/log/spamd.log

# Python
export PYTHONSTARTUP=$MEINE/python-init.py
export DJANGO_COLORS="nocolor"

# SystemDee
# Behave, you fuck.
export SYSTEMD_PAGER=
export SYSTEMD_COLORS=0

# GNU Coreutils
# Make numbered backups
export VERSION_CONTROL=numbered
