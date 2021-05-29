#!/usr/bin/env bash
# emacs-maybe-client.bash --- use emacsclient if server is live, start emacs otherwise

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

if emacsclient -e '(server-running-p)' 1>/dev/null 2>/dev/null; then
    exec emacsclient -c \
         --eval '(ignore-errors (find-file initial-buffer-choice))' \
         --eval '(raise-frame)' \
         $@
else
    exec emacs $@
fi
