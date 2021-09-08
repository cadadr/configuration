#!/bin/sh
# elisp-byte-compile.sh --- byte compile Emacs Lisp files

exec emacs -Q --batch -l ~/.emacs.d/loadpaths.el \
     -f batch-byte-compile "$@"
