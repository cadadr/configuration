# paths.sh --- User $PATH settings.

export CDPATH=.:$HOME:$HOME/Sources

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

# Take settings from local::lib but don’t let it set $PATH.
PERL_PATH="$HOME/perl5/bin"
eval "$(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib 2>/dev/null | grep -v '^PATH')" || :

# R
export R_LIBS_USER="$HOME/.local/share/R/site-library"

# Guile
export GUILE_LOAD_PATH="$MYLIB/scheme${GUILE_LOAD_PATH:+:$GUILE_LOAD_PATH}"

# Lua
export LUA_PATH="$MYLIB/lua/?.lua"

# Ruby
export GEM_HOME="$HOME/.gem"

if which ruby 2>&1 > /dev/null; then
    export GEM_PATH="$GEM_HOME/bin:$GEM_HOME/ruby/$(ruby -v | cut -d\  -f2 | cut -c1-3).0"
    RUBY_PATH="$GEM_PATH/bin"
fi

# Pyenv
export PYENV_ROOT="$MY/share/pyenv"
if [ -d "$PYENV_ROOT" ]; then
    export PATH="$PYENV_ROOT/bin:$PATH"
    if command -v pyenv 1>/dev/null 2>&1; then
        eval "$(pyenv init -)"
    fi
fi

# Pip installs to `.local'.
PKGMANS_PATH="$PERL_PATH:$RUBY_PATH:$HOME/.local/bin"

# Haskell
PKGMANS_PATH="$PKGMANS_PATH:$HOME/.cabal/bin:$HOME/.ghcup/bin"

# Add paths from ~/Apps.

# These are portable applications that can be run on multiple systems
# w/o recompilation, and is sort of parallel to how /opt is generally
# used.

# This is also where AppImages should be installed.

GK_OPT_DIR="$HOME/Apps"
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

# Man path
export MANPATH="$MY/doc/man:"   # keep the trailing (:), cf man(1)

# Finalise
export GK_PATHS_LOADED=yes
