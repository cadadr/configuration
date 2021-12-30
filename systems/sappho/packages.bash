#!/usr/bin/env bash
# packages.bash --- install void packages

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

# Desktop
xbps-install -y elogind tlp linux-firmware-intel mesa-dri vulkan-loader \
             mesa-vulkan-intel intel-video-accel pipewire xorg kitty

# Network
xbps-install -y avahi nss-mdns

# Development
xbps-install -y git sbcl

# Enable services
ln -s /{etc/sv,var/service}/avahi-daemon && sv up avahi-daemon
ln -s /{etc/sv,var/service}/tlp && sv up tlp

