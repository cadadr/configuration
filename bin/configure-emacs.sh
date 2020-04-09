#!/bin/sh

V=${1-head}
TK=${TK-yes}

if [ ! -f configure ]; then
    ./autogen.sh all
fi

case $V in
    nativecomp)
	./configure \
	    --prefix=$HOME/local/emacs \
	    --with-x-toolkit=$TK \
	    --with-modules \
	    --with-file-notification=yes \
	    --with-gameuser=no \
            --with-cairo \
	    --with-mailutils \
            --with-imagemagick \
            \
            --with-nativecomp ;;
    head)
	./configure \
	    --prefix=$HOME/local/emacs \
	    --with-x-toolkit=$TK \
	    --with-modules \
	    --with-file-notification=yes \
	    --with-gameuser=no \
            --with-cairo \
	    --with-mailutils \
            --with-imagemagick ;;
    nothreads)
	./configure \
	    --prefix=$HOME/local/emacs \
	    --with-x-toolkit=$TK \
	    --with-modules \
	    --with-file-notification=yes \
	    --with-gameuser=no \
	    --with-mailutils \
	    --without-threads ;;
    26)
	./configure \
	    --prefix=$HOME/local/emacs \
	    --with-x-toolkit=$TK \
	    --with-modules \
	    --with-file-notification=yes \
	    --with-mailutils ;;
    25)
	./configure \
	    --prefix=$HOME/local/emacs \
	    --with-x-toolkit=$TK \
	    --with-modules \
	    --with-file-notification=yes ;;
    *) echo "I don't know how to configure Emacs version $1"; exit 1;;
esac
