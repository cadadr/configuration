# env.sh --- Shell environment.

export MYFS=/igk

export EMAIL=$USER@localhost
export MAILDIR=$HOME/posta
export MAILQUEUE=$MAILDIR/queue
export PS1='\$ '
export PASSWORD_STORE_DIR=$HOME/fil/pass
export TROFFONTS=$HOME/.fonts

# Fool shitty apps to believe that this is a desktop environment.
# Simple Scan won't allow keybindings otherwise.
export XDG_CURRENT_DESKTOP=XFCE

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

export CVSROOT=$MYFS/cvsroot
export GITROOT=/var/git
export HGROOT=/var/mercurial

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

# Golang
export GOPATH=$HOME/co/Go
export GO=$GOPATH/bin

# Python
export PYTHONSTARTUP=$MEINE/python-init.py
export DJANGO_COLORS="nocolor"

# GNU Coreutils

# Make numbered backups
export VERSION_CONTROL=numbered
