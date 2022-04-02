#!/usr/bin/env bash
# zotero-appimage.bash --- generate zotero appimage

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

# Adapted from: https://forums.zotero.org/discussion/83039/appimage-for-linux

### Create a build directory:
OLDPWD="$PWD"
BUILDDIR="$(mktemp --tmpdir --directory zotero-appimage-buildXXXXXXXXXX)"

cd "$BUILDDIR"

echo Building in $PWD...

if [ "$PWD" != "$BUILDDIR" ]; then
   echo "Failed to cd to build directory at $BUILDDIR"
   exit 1
fi


### Download and prepare necessary files:
arch="$(uname -m)"
version="6.0.4"
dlurlbase="https://www.zotero.org/download/client/dl?channel=release&platform=linux-$arch&version="
dlurl="$dlurlbase$version"
dlname="Zotero-${version}_linux-$arch.tar.bz2"
distdir="Zotero-${version}_linux-$arch"

appiurl="https://github.com/AppImage/AppImageKit/releases/download/continuous/appimagetool-$arch.AppImage"
appitool="appimagetool-$arch.AppImage"

image="Zotero-${version}_linux-$arch.AppImage"

echo Downloading $appitool and $dlname...

# Download appimagetool first because if this fails there’s not much
# use to the rest of this script.
wget -nv -O "$appitool" "$appiurl"
chmod +x "$appitool"

# Download Zotero tarball
wget -nv -O "$dlname" "$dlurl"

echo Unpacking $dlname...

tar -xf "$dlname"
mv "Zotero_linux-$arch" "$distdir"

### Transform tree to AppImage format:
echo Transforming $distdir...

(
    cd "$distdir" ;

    # Executable
    mv zotero AppRun ;

    # .desktop file
    sed "s/Exec=.*/Exec=zotero-bin -url %U/g" zotero.desktop \
        | sed 's/^SingleMainWindow/X-SingleMainWindow/'      \
        | sed "s/zotero.ico/zotero/g" > zotero-new.desktop ;
    mv zotero-new.desktop zotero.desktop ;

    # Icons
    cp chrome/icons/default/main-window.ico zotero.ico ;
    convert zotero.ico zotero.png ;
    mv zotero-0.png zotero.png ;
    rm zotero-*.png zotero.ico ;

    # Disable auto-opdate
    sed -e '/app.update.enabled",/s/true/false/' \
        -e '/app.update.auto",/s/true/false/'    \
        < defaults/preferences/prefs.js          \
        > prefs-new.js                           ;
    mv prefs-new.js defaults/preferences/prefs.js ;
)

### Build the AppImage
echo Building $image...

"./$appitool" "$distdir"
mv "Zotero-$arch.AppImage" "$image"

echo "Done: $PWD/$image"
