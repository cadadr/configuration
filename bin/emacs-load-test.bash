#!/usr/bin/env bash
# emacs-load-test.bash --- emacs load test

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

EMACS="${EMACS-emacs}"

echo
echo "EMACS=$EMACS"
echo

# XXX(2020-06-01): ‘-d ''’ doesn’t work for some reason.
read -r -d ';;;' expression <<END
(let
    ((time-start (current-time)))
  (message
   "Startup took %f seconds..."
   (car (benchmark-run 1
          (progn
            (load (expand-file-name "~/.emacs.d/init.el"))
            (message "-OK-"))))));;;
END

exec "$EMACS" --batch --eval="$expression"
