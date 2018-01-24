#!/bin/sh
# Build the packages in src/ and install.

# exit on error
set -e

emacsdir="${HOME}/.emacs.d"
cd $emacsdir

### Emacs build and installation:
emacsver='24.5'
emacsopts="--with-sound=yes --with-x-toolkit=yes --with-wide-int \
        --with-file-notification=yes --without-compress-install\
        --with-x"

# When emacs 25 add also --with-xwidgets

cd $emacsdir

buildemacs () {
    cd src/emacs-$emacsver

    [ -f 'Makefile' ] && make distclean
    [ -d 'build' ] && rm -rf build

    mkdir -p build
    cd build
    ../configure --quiet --prefix=$emacsdir/emacs-$emacsver\
		 --infodir=$emacsdir/docs/emacs-$emacsver\
		 --bindir=$emacsdir/bin\
		 --enable-silent-rules\
		 --disable-dependency-tracking $emacsopts

    cd ..
    cd $emacsdir
}

installemacs () {
    cd src/emacs-$emacsver/build || return 1
    make install
    make distclean
    rm -rf build
}

### Pdftools:
pdfver='0.70'

buildpdftools () {
    cd src/pdf-tools-$pdfver
    make distclean
    make
    cd $emacsdir
}

installpdftools () {
    cd src/pdf-tools-$pdfver
    rm -rf $emacsdir/packages/pdf-tools-$pdfver
    mv pdf-tools-$pdfver $emacsdir/packages
    make distclean
    cd $emacsdir
}

buildall () {
    buildemacs
    buildpdftools
}

iemacs () {
    buildemacs
    installemacs
}

ipdftools () {
    buildpdftools
    installpdftools
}

help () {
    echo "usage: $0 <option>"
    echo 'options:'
    echo '	all		build all'
    echo '	emacs		build emacs'
    echo '	pdftools	build pdftools'
    echo '	install		build and install packages.'
    echo '	iemacs		build and install emacs'
    echo '	ipdftools	build and install pdftools'
}

case $1 in
    all) buildall ;;
    emacs) buildemacs ;;
    pdftools) buildpdftools ;;
    install) iemacs && ipdftools ;;
    iemacs) iemacs ;;
    ipdftools) ipdftools ;;
    *) help ;;
esac
