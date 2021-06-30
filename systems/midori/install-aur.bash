#!/usr/bin/env bash
# install-aur.bash --- install AUR packages in correct order

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

# TODO: psychopy duplicati
packages="ttf-sil-doulos
ttf-charis-sil
praat
duplicati
notify-sharp"

for pkg in $packages; do
    (
        cd pkgbuilds/$pkg;
        rm -f *.tar.zst;
        makepkg -s;
        sudo pacman -U *.tar.zst
    )
done
