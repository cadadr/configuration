# paths.sh --- User $PATH settings.

export CDPATH=.:$HOME:$HOME/co

PROFILE_PATH="$HOME/bin"

# Collect local path
LOCALDIR=$HOME/local
LOCALPATH=

if [ -d $LOCALDIR ]; then
    for dir in $(ls $LOCALDIR); do
	d=$LOCALDIR/$dir
	if [ -d $d/bin ]; then
	    LOCALPATH=$d/bin:$LOCALPATH
	fi
    done

    PROFILE_PATH="$PROFILE_PATH:$LOCALPATH"
fi

# Deduplicate path.

PATH=$(echo "$PATH" | tr ':' '\n' | grep '^/[^h]' | sort | uniq | tr '\n' :)

# Collect paths from programming-language specific package managers.

export GEM_HOME="$HOME/.gem"
export GEM_PATH="$GEM_HOME/bin:$GEM_HOME/ruby/$(ruby -v | cut -d\  -f2 | cut -c1-3).0"

if which ruby 2>&1 > /dev/null; then
    RUBY_PATH="$GEM_PATH/bin"
fi

# Pip installs to `.local'.
PKGMANS_PATH="$RUBY_PATH:$HOME/.local/bin"

# Add paths from ~/opt.

# These are portable applications that can be run on multiple systems
# w/o recompilation, and is sort of parallel to how /opt is generally
# used.

GK_OPT_DIR="$HOME/opt"
ANDROID_HOME="$GK_OPT_DIR/android-sdk";		export ANDROID_HOME
ANDROID_SDK_HOME="$HOME/.android";		export ANDROID_SDK_HOME
GK_OPT_PATH="$GK_OPT_DIR/flutter/bin"

# Finalise binary path.

PATH="$PROFILE_PATH:$PKGMANS_PATH:$GK_OPT_PATH:$PATH"
# Remove double colons.
PATH="$(echo $PATH | sed -E s,:+,:,g)"
export PATH
