# profile -- Login shells.

### Import system settings:
# . /etc/profile >/dev/null 2>&1 || echo Could not source /etc/profile...
[ -f .guix-profile/etc/profile ] && . .guix-profile/etc/profile

###

### Locale and time zone:
MM_CHARSET=UTF-8;		export MM_CHARSET
LANG=en_GB.$MM_CHARSET;		export LANG
LANGUAGE=$LANG;			export LANGUAGE
LC_MONETARY=tr_TR.$MM_CHARSET;	export LC_MONETARY
LC_TIME=tr_TR.$MM_CHARSET;	export LC_TIME

# Timezone
TZ=Europe/Istanbul;	export TZ

###

### Session:

# SSH agent:
tmpfil=$(mktemp)
ssh-agent>$tmpfil
. $tmpfil

# Bookkeeping to use notify-send from processess started outside the X
# session.  Mainly cron.
dbusfil=/tmp/${USER}-dbus
touch $dbusfil
chmod 600 $dbusfil
env | grep DBUS_SESSION_BUS_ADDRESS > $dbusfil
echo 'export DBUS_SESSION_BUS_ADDRESS' >> $dbusfil

###

### Variables and system information:

export MEINE=/igk/config/dotfiles
export SYSTEM=$(uname)

###

### Paths:

PROFILE_PATH="$HOME/bin:$HOME/local/bin"

# Guix.
if [ -d "$HOME/.guix-profile" ]; then
    export GUIX_LOCPATH=$HOME/.guix-profile/lib/locale
    export GUILE_WARN_DEPRECATED=detailed
    GUIX_PATH=$HOME/.guix-profile/bin:$HOME/.guix-profile/sbin

    export GIT_EXEC_PATH="/home/g/.guix-profile/libexec/git-core"
    export PYTHONPATH="/home/g/.guix-profile/lib/python3.5/site-packages${PYTHONPATH:+:}$PYTHONPATH"
    export PERL5LIB="/home/g/.guix-profile/lib/perl5/site_perl${PERL5LIB:+:}$PERL5LIB"
    export GEM_PATH="/home/g/.guix-profile/lib/ruby/gems/2.4.0${GEM_PATH:+:}$GEM_PATH"
    export XDG_DATA_DIRS="/home/g/.guix-profile/share${XDG_DATA_DIRS:+:}$XDG_DATA_DIRS"
    export ASPELL_DICT_DIR="/home/g/.guix-profile/lib/aspell"
    export INFOPATH="/home/g/.guix-profile/share/info${INFOPATH:+:}$INFOPATH"
    export ACLOCAL_PATH="/home/g/.guix-profile/share/aclocal${ACLOCAL_PATH:+:}$ACLOCAL_PATH"
    export PKG_CONFIG_PATH="/home/g/.guix-profile/lib/pkgconfig${PKG_CONFIG_PATH:+:}$PKG_CONFIG_PATH"
fi

# Deduplicate path.

PATH=$(echo "$PATH" | tr ':' '\n' | grep '^/[^h]' | sort | uniq | tr '\n' :)

# Collect paths from programming-language specific package managers.
if which ruby 2>&1 > /dev/null; then
    RUBY_PATH="$HOME/.gem/ruby/$(ruby -v | cut -d\  -f2 | cut -c1-3).0/bin"
fi

# Pip installs to `.local'.
PKGMANS_PATH="$RUBY_PATH:$HOME/.local/bin"

PATH="$PROFILE_PATH:${PATH}$GUIX_PATH:$PKGMANS_PATH"
# Remove double colons.
PATH="$(echo $PATH | sed -E s,:+,:,g)"
export PATH

###

### Environment:

export EMAIL=$USER@localhost
export MAILDIR=$HOME/Mail
export PS1='\$ '
export PASSWORD_STORE_DIR=$HOME/fil/pass
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

export CVSROOT=/igk/cvsroot
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
export SPAMD_PID=$HOME/.spamd.pid
export SPAMD_LOG=$HOME/.spamd.log

# Golang
export GOPATH=$HOME/co/Go
export GO=$GOPATH/bin

# Python
export PYTHONSTARTUP=$MEINE/python-init.py
export DJANGO_COLORS="nocolor"

###

### Umask:
umask 027

###

### Import shell settings:
# set ENV to a file invoked each time sh is started for interactive use.
ENV=$HOME/.$(basename $SHELL)rc; export ENV
export FROMLOGINPROFILE=yes
. $ENV
unset FROMLOGINPROFILE

