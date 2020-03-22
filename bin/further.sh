#!/bin/sh
# further.sh --- find items in ~/Notes tree marked for further consideration.

echo '# -*- mode: grep-mode -*-'
echo
date
hostname -f
echo
find ~/Notes -name \*.org -exec grep -Hn -C 1 -- '- ->' \{\} \;
