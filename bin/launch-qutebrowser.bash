#!/usr/bin/env bash
# launch-qutebrowser.bash --- launch qutebrowser from virtual environment

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

export PATH="$HOME/local/_qutebrowser/bin/:$PATH"
. $HOME/local/_qutebrowser/bin/activate


# Workaround from
# https://github.com/qutebrowser/qutebrowser/issues/5656#issuecomment-761821220

rm -frv ~/.local/share/qutebrowser/webengine/Service\ Worker/CacheStorage

exec ~/local/_qutebrowser/bin/python3 -m qutebrowser --basedir "$MY/qutebrowser" "$@"

