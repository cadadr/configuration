#!/bin/sh
# kvm.sh --- set up user for kvm accel

[ "$(id -u)" -le 999 ] \
	&& { echo "run script as a regular user"; exit 1; }

[ $(egrep -c '(vmx|svm)' /proc/cpuinfo) -ge 1 ] \
	|| { echo "can not use kvm"; exit 2; }

sudo apt-get install qemu-kvm libvirt-daemon-system \
	libvirt-clients bridge-utils

# for android?
# sudo apt-get install libvirt-bin ubuntu-vm-builder ia32-libs-multiarch

sudo adduser $(id -un) libvirt
sudo adduser $(id -un) kvm

