#!/bin/bash
# installer.bash --- install Linux Mint on USB device using Qemu

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

QEMU="${QEMU-qemu-system-x86_64}"
INSTALL_IMG="${INSTALL_IMG-/media/g/igk-attic/Installation Images/linuxmint-20-cinnamon-64bit.iso}" 
INSTALL_DEVICE="${INSTALL_DEVICE-/dev/sdb}"

exec "$QEMU" \
	-boot order=d -m 1024 -enable-kvm -smp 2 -cpu host \
	-cdrom "$INSTALL_IMG" \
	-hda "$INSTALL_DEVICE"
