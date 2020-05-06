#!/bin/sh
# journal.sh

fil="$HOME/Documents/not/journal/$(date +%F)"

date > $fil

exec $EDITOR $fil

