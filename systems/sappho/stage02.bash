#!/usr/bin/env bash
# stage02.bash --- prepare the OS for user setup

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

if [ "$UID" != 0 ]; then
    echo root privileges required
    exit 1
fi

### Config:
username=cadadr

# Allow non-root users to use ping because why not?
sysctl -w net.ipv4.ping_group_range="0 1000"

# Claim ownership of home directory.
( cd ; chown $username:$username . )

### Install packages:

# Update repositories
xbps-install -Sy

# Basics and utilities
xbps-install -y bc bind-utils djvulibre entr inotify-tools jq \
             lm_sensors moreutils num-utils pv smartmontools tmux \
             units vim-common qrencode xtools udisks2 atop dcron \
             void-repo-nonfree

# Update repositories after installing ‘void-repo-nonfree’
xbps-install -Sy

# Shells
xbps-install -y zsh

# Drivers
xbps-install -y v4l2loopback hidapi ldacBT broadcom-bt-firmware

# Backups
xbps-install -y borg rclone

# Crypto, privacy, security
xbps-install -y gnupg pass pwgen

# Language and locales
xbps-install -y hunspell hunspell-{de_DE,en_GB,en_US,es_ES,fr_FR,it_IT} \
                hunspell-pt_PT libreoffice-i18n-{en-GB,en-US,tr}

# Desktop [common/X11]
# (libnotify contains notify-send)
xbps-install -y elogind tlp linux-firmware-intel mesa-dri vulkan-loader \
             mesa-vulkan-intel intel-video-accel pulseaudio xorg   \
             i3-gaps i3status rofi scrcpy volctl libnotify xclip \
             xdotool xpra fontconfig flatpak dunst bluez compton \
             feh pinentry-gtk geoclue2 xss-lock i3lock udiskie arandr \
             tumbler sound-theme-freedesktop adwaita-plus

# TODO: Desktop [Wayland]
# hikari, ...

# Desktop apps
xbps-install -y vokoscreen kitty audacity cheese clipit dconf-editor \
             dex gsmartcontrol inkscape kdeconnect libreoffice \
             mpv quodlibet simple-scan transmission-gtk atril kdenlive \
             gnome-font-viewer firefox flameshot network-manager-applet \
             pcmanfm pavucontrol blueman ristretto mate-system-monitor \
             gvim-huge

# Audio
xbps-install -y vorbisgain vorbis-tools

# Fonts
xbps-install -y \
             ttf-ubuntu-font-family ttf-bitstream-vera dejavu-fonts-ttf \
             freefont-ttf fonts-roboto-ttf liberation-fonts-ttf \
             noto-fonts-ttf noto-fonts-ttf-extra font-libertine-otf \
             Fonts-TLWG amiri-font cantarell-fonts ekushey-fonts-ttf \
             font-awesome5 font-fantasque-sans-ttf font-hanazono \
             font-inconsolata-otf terminus-font font-sun-misc font-vazir \
             fonts-croscore-ttf gsfonts grub-terminus noto-fonts-cjk \
             noto-fonts-emoji twemoji

# (fuck SIL, boring dickheaded assholes, disgrace of linguistics)
eval "xbps-install -y $(xbps-query -Rs font-sil- | cut -d ' ' -f 2 | tr '\n' ' ')"

# Network
xbps-install -y avahi curl nss-mdns openbsd-netcat net-tools avahi-utils

# Internet & mail
# (inetutils contains telnet, whois, and traceroute)
xbps-install -y lynx mairix mb2md mpop msmtp mutt procmail inetutils \
             wget yt-dlp links

# Scientific & academic
xbps-install -y gnuplot praat

# Data
xbps-install -y sqlite

# Document processing & authoring
xbps-install -y pandoc pdftk tidy5 wkhtmltopdf unoconv pdfpc

# Programming
xbps-install -y base-devel cmake autoconf autoconf-archive automake \
             bmake cpanminus ctags gdb gcc-fortran guile ghc hoogle \
             python3-ipython R ruby-ri ruby strace swi-prolog \
             yarn python3-pip python3-setuptools python3-tkinter \
             python3-wheel sbcl vala valadoc pkgconf

# Libraries
xbps-install -y python3-devel ruby-devel perl-Switch perl-local-lib \
             perl-JSON perl-Reply pdf.js openssl-devel fuse-devel \
             libcurl-devel libgcc-devel libgirepository-devel \
             ImageMagick libpng-devel poppler-devel poppler-glib-devel \
             zlib-devel

# VCS
xbps-install -y git cvs cvsps2 git-cvs mercurial hg-git quilt rcs \
             darcs subversion

# Emulation
xbps-install -y qemu

# Nonfree
xbps-install -y intel-ucode

# Update user groups
usermod -aG kvm $username

# Add flatpak remote
flatpak remote-add --if-not-exists flathub https://flathub.org/repo/flathub.flatpakrepo

### Enable services:

ln -s /{etc/sv,var/service}/avahi-daemon && sv up avahi-daemon
ln -s /{etc/sv,var/service}/tlp && sv up tlp
ln -s /{etc/sv,var/service}/sshd && sv up sshd
ln -s /{etc/sv,var/service}/bluetoothd && sv up bluetoothd
ln -s /{etc/sv,var/service}/dcron && sv up dcron

### Enable kernel modules:

cat <<EOF > /etc/modules-load.d/sappho.conf
kvm-intel
v4l2loopback
EOF

### Extend locales:

echo tr_TR.UTF-8 UTF-8 >> /etc/default/libc-locales
xbps-reconfigure -f glibc-locales

### Configure avahi/mdns:

# Disable IPv6, use v4 only, causes trouble with connectivity.
sed -E -i.bkp 's/^#?use-ipv6=yes/use-ipv6=no/' /etc/avahi/avahi-daemon.conf
sed -i.bkp    's/mdns/mdns4_minimal [NOTFOUND=return]/' /etc/nsswitch.conf

### Install configuration files:

install -b -o root -g root -m 644 udev.rules /etc/udev/rules.d/sappho.rules

