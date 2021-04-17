#!/usr/bin/env bash
# launch-qutebrowser.bash --- launch qutebrowser from virtual environment

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

export PATH="$HOME/local/_qutebrowser/bin/:$PATH"
. $HOME/local/_qutebrowser/bin/activate
exec qutebrowser
