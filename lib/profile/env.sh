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

case $SYSTEM in
    FreeBSD)
	export SSL_CERT_DIR='/etc/ssl';
	export SSL_CERT_FILE="$SSL_CERT_DIR/cert.pem";
	export TERMINFO=/usr/local/share/misc/terminfo.db;;
    Linux)
	export SSL_CERT_DIR='/etc/ssl/certs';
	export SSL_CERT_FILE="$SSL_CERT_DIR/ca-certificates.crt";;
esac

# This made me spend months trying to download the CVS repo of
# OpenBSD...
export CVS_RSH=ssh

# Email
export NOTMUCH_CONFIG=$MEINE/mail/notmuch.ini

# Make.
export MAKEOBJDIRPREFIX=$HOME/obj

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

# Pyenv
export PYENV_ROOT="$MY/share/pyenv"
if [ -d "$PYENV_ROOT" ]; then
    export PATH="$PYENV_ROOT/bin:$PATH"
    if command -v pyenv 1>/dev/null 2>&1; then
        eval "$(pyenv init -)"
    fi
fi

# R
export R_LIBS_USER="$HOME/.local/share/R/site-library"

# Guile
export GUILE_LOAD_PATH=$MYLIB/scheme:$GUILE_LOAD_PATH

# Lua
export LUA_PATH="$MYLIB/lua/?.lua"

# Perl
eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"

# SystemDee
# Behave, you fuck.
export SYSTEMD_PAGER=
export SYSTEMD_COLORS=0

# GNU Coreutils
# Make numbered backups
export VERSION_CONTROL=numbered

# Man path
export MANPATH="$MY/doc/man:"

# XDG

# Push flatpak to the end.
flatpaks="$(echo $XDG_DATA_DIRS | awk -v RS=: '/flatpak/{print $0}')"
others="$(echo $XDG_DATA_DIRS | awk -v RS=: '/flatpak/{next}; 1')"

export XDG_DATA_DIRS="$(echo $others $flatpaks | tr ' ' :)"

unset flatpaks others
