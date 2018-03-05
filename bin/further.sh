#!/bin/sh
# further.sh --- find items in ~/doc/not tree marked for further consideration.

echo '# -*- mode: grep-mode -*-'
echo
date
hostname -f
echo
find ~/doc/not/ -name \*.org -exec grep -Hn -C 1 -- '- ->' \{\} \;
