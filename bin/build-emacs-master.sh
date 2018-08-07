#!/bin/sh
# build-emacs-master.sh --- fetch, review, merge and build Emacs master branch.

set -e

cd ~/co/External/emacs

git branch | grep '^\* master' >/dev/null || exit 'Not on branch master!'

git fetch

echo News since HEAD:
git news

echo
echo Commits since HEAD:
git wut

echo 'Continue with build (RETURN to continue, ^C to quit)?'; read

git pull --ff
git clean -dfx

TK=gtk configure-emacs.sh

make

echo Build finished, hit RETURN to continue to testing w/ -Q.; read
./src/emacs -Q

echo Hit RETURN to test w/ configuration.; read
./src/emacs

echo Hit RETURN to install...; read
rm -rf ~/local/emacs && make install
