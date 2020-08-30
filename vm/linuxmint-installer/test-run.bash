#!/bin/bash
# test-run.bash --- test Linux Mint install of Qemu on external HDD

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

QEMU="${QEMU-qemu-system-x86_64}"
INSTALL_DEVICE="${INSTALL_DEVICE-/dev/sdb}"

exec "$QEMU" \
	-boot order=c -m 1024 -enable-kvm -smp 2 \
	-hda "$INSTALL_DEVICE"
