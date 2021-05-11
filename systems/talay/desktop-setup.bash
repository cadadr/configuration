#!/usr/bin/env bash
# desktop-setup.bash --- setup desktop session

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

bash $MYLIB/launch-session-servers.bash
