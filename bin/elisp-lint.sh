#!/bin/sh
# lint.sh --- elisp lint

exec emacs -Q --batch \
     -l ~/.emacs.d/loadpaths.el \
     -l ~/.emacs.d/lisp/site/misc/elisp-lint.el \
     -f elisp-lint-files-batch $@ 2>&1
