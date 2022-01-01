# paths.sh --- User $PATH settings.

export CDPATH=.:$HOME:$HOME/co

if [ -z "$DEFAULT_PATH" ]; then
    export DEFAULT_PATH="$PATH"
else
    PATH="$DEFAULT_PATH"
fi

PROFILE_PATH="$HOME/bin"

# Collect local path
LOCALDIR=$HOME/local
LOCALPATH=

if [ -d $LOCALDIR ]; then
    for d in $(find $LOCALDIR -maxdepth 1 -mindepth 1 -not -name '_*'); do
	if [ -d $d/bin ]; then
	    LOCALPATH=$d/bin:$LOCALPATH
	fi
    done

    PROFILE_PATH="$PROFILE_PATH:$LOCALPATH"
fi

# Collect paths from programming-language specific package managers.

# Perl
#
# This will install ~/perl5/bin to the front of $PATH, and set some
# other perl-related environment variables.
eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)"

# Ruby
export GEM_HOME="$HOME/.gem"

if which ruby 2>&1 > /dev/null; then
    export GEM_PATH="$GEM_HOME/bin:$GEM_HOME/ruby/$(ruby -v | cut -d\  -f2 | cut -c1-3).0"
    RUBY_PATH="$GEM_PATH/bin"
fi

# Pip installs to `.local'.
PKGMANS_PATH="$RUBY_PATH:$HOME/.local/bin"

# Haskell
PKGMANS_PATH="$PKGMANS_PATH:$HOME/.cabal/bin:$HOME/.ghcup/bin"

# Add paths from ~/opt.

# These are portable applications that can be run on multiple systems
# w/o recompilation, and is sort of parallel to how /opt is generally
# used.

GK_OPT_DIR="$HOME/Applications"
GK_OPT_PATH="$GK_OPT_DIR"

# Finalise binary path.

PATH="$PROFILE_PATH:$PKGMANS_PATH:$GK_OPT_PATH:$PATH"
# Remove double colons.
PATH="$(echo $PATH | sed -E s,:+,:,g)"
# Remove double slashes.
PATH="$(echo $PATH | sed -E s,/+,/,g)"
# Remove trailing colons.
PATH="$(echo $PATH | sed -E s,:+\$,,g)"
export PATH

if [ -e /etc/systemd ]; then
    # Update systemd paths.
    systemctl --user import-environment PATH
    systemctl --user import-environment GEM_HOME
    systemctl --user import-environment GEM_PATH
fi

# Finalise
export GK_PATHS_LOADED=yes
