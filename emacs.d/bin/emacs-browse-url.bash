#!/usr/bin/env bash
# emacs-browse.bash --- emacs browse-url interface for freedesktop

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

# HACK(2021-04-22): xdg-open cannot handle %-codes when they are not
# space-separated.
# see: https://github.com/freedesktop/xdg-utils/blob/d11b33ec7f24cfb1546f6b459611d440013bdc72/scripts/xdg-open.in#L296-L320

. $MYLIB/fns.sh

say browsing $1

emacsclient -c --eval "(browse-url \"$1\")"
