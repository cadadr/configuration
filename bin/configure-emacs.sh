#!/bin/sh

V=${1-head}
TK=${2-yes}

if [ ! -f configure ]; then
    ./autogen.sh all
fi

# Keep an eye on:
#   . --with-cairo
#   . Threads' development, enable when stable
case $V in
    head)
	./configure \
	    --prefix=$HOME/local \
	    --with-x-toolkit=$TK \
	    --with-modules \
	    --with-file-notification=yes \
	    --without-threads \
	    --with-gameuser=no \
	    --with-mailutils ;;
    26)
	./configure \
	    --prefix=$HOME/local \
	    --with-x-toolkit=$TK \
	    --with-modules \
	    --with-file-notification=yes \
	    --with-mailutils ;;
    25)
	./configure \
	    --prefix=$HOME/local \
	    --with-x-toolkit=$TK \
	    --with-modules \
	    --with-file-notification=yes ;;
    *) echo "I don't know how to configure Emacs version $1"; exit 1;;
esac
