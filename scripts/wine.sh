# wine.sh --- install wine

# Adapted from https://wiki.debian.org/Wine

dpkg --add-architecture i386 && apt-get update

# Standard 64bit install
apt-get install wine wine32 wine64 libwine libwine:i386 fonts-wine
