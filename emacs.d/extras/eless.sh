#!/bin/sh
# A script for $PAGER that redirects to an Emacs buffer
# Adapted from https://crowding.github.io/blog/2014/08/16/replace-less-with-emacs/

set -e

# make a named fifo
FIFO=$(mktemp -ut pagerXXXXXXXXXXX.$$)
mkfifo $FIFO

emacsclient -u -e "(gk-less \"$FIFO\")"

exec cat > "$FIFO"
