#!/bin/sh

V=${1-head}
TK=${TK-yes}

if [ ! -f configure ]; then
    ./autogen.sh all
fi

# Keep an eye on:
#   . --with-cairo
case $V in
    pdmp)
	./configure \
	    --prefix=$HOME/local \
	    --with-x-toolkit=$TK \
	    --with-imagemagick \
	    --with-modules \
	    --with-file-notification=yes \
	    --with-gameuser=no \
	    --with-mailutils \
	    \
	    --with-pdumper=yes \
	    --with-unexec=yes \
	    --with-dumping=pdumper ;;
    head)
	./configure \
	    --prefix=$HOME/local \
	    --with-x-toolkit=$TK \
	    --with-imagemagick \
	    --with-modules \
	    --with-file-notification=yes \
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
