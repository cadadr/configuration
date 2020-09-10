#!/bin/bash
# installer.bash --- install Linux Mint on USB device using Qemu

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

QEMU="${QEMU-qemu-system-x86_64}"
QOPTS="-boot order=d -m 1024 -enable-kvm -smp 2 -cpu host"
QSSH="-net user,hostfwd=tcp::10022-:22 -net nic"
BOOT="${INSTALL_IMG-$HOME/Downloads/archlinux-2020.09.01-x86_64.iso}"
ROOT=/tmp/disk.img

CMD="$QEMU $QOPTS $QSSH -hda $ROOT"

help(){
    echo "$0: usage: $0 [help | install | boot]"
}

# If disk image doesn’t exist boot in install mode, else boot from the
# disk image.
if [ "${1-nope}" = "help" ]; then
    help
elif [ ! -e "$ROOT" -o "${1-nope}" = "install" ]; then
    echo Install mode booting...
    [ -e "$ROOT" ] || qemu-img create -f qcow "$ROOT" 20G
    eval $CMD -cdrom "$BOOT"
elif [ "${1-nope}" = "boot" ]; then
    echo Use mode booting...
    eval $CMD
else
    help
fi
