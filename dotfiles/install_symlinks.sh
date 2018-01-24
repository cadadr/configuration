#!/bin/sh
#
# install_symlinks.sh -- Install symlinks into home directory
#
# Symlinks are specified in the `map' file like this:
#
#   link-name target
#   link-name target
#      .
#      .
#
# This script creates specified symbolic links.  `link-name' should be
# absolute, `target' relative to the directory of this script, the two
# is seperated by a space.
#
# Initial tildes (~)  in link-names are substituted with  the value of
# $HOME.

while read line; do
    link="$(echo $line | cut -f1 -d ' ' | sed s:^~:$HOME:)"
    target="$PWD/$(echo $line | cut -f2 -d ' ')"
    mkdir -p "$(dirname $link)"
    if [ -f $link ]; then mv $link $link~before-symlink; fi
    ln -sfv $target $link
    chmod -v 640 $link
done < map
