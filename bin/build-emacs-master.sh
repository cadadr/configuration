#!/bin/sh
# build-emacs-master.sh --- fetch, review, merge and build Emacs master branch.

# This script is for pulling and building Emacs master branch
# interactively.  It depends on the ‘configure-emacs.sh’ and
# ‘emacs-load-test.bash’ scripts that reside in this same directory.
# Ideally, you should download these three scripts together and put
# them in to some directory which is in the $PATH.  Or, you can read
# the comments found inline which should help make it work without
# those scripts.

set -e

# Path to emacs git repo clone
cd ~/co/External/emacs

git branch | grep '^\* master' >/dev/null || exit 'Not on branch master!'

git fetch

echo News since HEAD:
git diff HEAD..FETCH_HEAD -- etc/NEWS

echo
echo Commits since HEAD:
git log --graph --oneline HEAD..FETCH_HEAD

echo 'Continue with build (RETURN to continue, ^C to quit)?'; read nought

git pull --ff-only
git clean -dfx

# Run the configure script.
if [ ! -f configure ]; then
    ./autogen.sh all
fi

./configure --prefix=$HOME/local/emacs    \
            --with-x-toolkit=${TK-athena} \
            --with-modules                \
            --with-file-notification=yes  \
            --with-gameuser=no            \
            --with-cairo                  \
            --with-harfbuzz               \
            --with-mailutils              \
            --disable-build-details       \
            --with-json                   \
            --without-compress-install    \
            --with-native-compilation

echo Configure script completed, review output and hit RETURN to build; read nought

make -j$(lscpu | awk '/^CPU\(s\):/ {print $2}')

echo Build finished, hit RETURN to continue to testing w/ -Q.; read nought
./src/emacs -Q

# Test if the compiled Emacs can load init.el.  The
# ‘emacs-load-test.sh’ should be in the PATH.  You can change the
# script file name, or just run ‘./src/emacs’ and ‘C-x C-c’ out of it
# if the config loads fine.
echo Hit RETURN to test w/ configuration.; read nought
EMACS=./src/emacs emacs-load-test.bash


echo Hit RETURN to install...; read nought
rm -rf ~/local/emacs && make install
