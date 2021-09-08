#!/bin/sh
# nonrequired.sh -- Find emacs modules never required.
#
# Useful for who wants to clean their ~/.emacs.d up.  Lists up the
# modules never required, naively.  Assumes that you always require
# like this:
#
#   (require 'module)
#
# Note about org files:
#
# Many org stuff ends up in the list, mostly contribs and org-babel
# stuff.  To exclude all that, pipe the output from this script to
# this command:
#
#   egrep -v '^o(rg|b|x)-'
#
# Also BBDB and pdf-tools package some files never required, so if you
# have them, extend the pipe like this:
#
#   nonrequired.sh | egrep -v '^o(rg|b|x)-' | egrep -v '^(bbdb|pdf)-'
#
# Expand the pipeline as needed.

set -e

# Make sure there's a trailing slash.
emacsdir=$HOME/.emacs.d/
modules=$(mktemp)
required=$(mktemp)

find $emacsdir -name '*.el' | xargs grep "(provide '" | cut -d: -f2 \
    | sed -E "s/^.*'([A-Za-z0-9-]+).*$/\\1/" \
    | sort > $modules

find $emacsdir -name '*.el' | xargs grep "(require '" | cut -d: -f2 \
    | sed -E "s/^.*'([A-Za-z0-9-]+).*$/\\1/" \
    | sort | uniq > $required

while read module;
do
    grep "$module" $required > /dev/null || echo $module;
done < $modules
