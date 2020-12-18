#!/usr/bin/env bash
# boot-setup.bash --- enable hibernation and GRUB menu under ubuntu et al.

# *Note* that this requires a good amount of swap space, preferably
# 2*RAM.

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

# Check for superuser priviledges
if [ $UID -ne 0 ]; then
    echo This script requires superuser priviledges\!
    exit 2
fi


# Enable boot menu with some timeout
# Adapted from: https://livingthelinuxlifestyle.wordpress.com/2020/10/21/show-the-grub-boot-menu-in-linux-mint-at-startup/
printf "GRUB_TIMEOUT_STYLE=menu\nGRUB_TIMEOUT=30\n" \
    | tee /etc/default/grub.d/99_showmenu.cfg > /dev/null


# Enable resuming from hibernation
# Adapted from: https://askubuntu.com/a/1241902
swapuuid="$(grep ^UUID=.*swap /etc/fstab | cut -d ' ' -f 1)"
echo "GRUB_CMDLINE_LINUX_DEFAULT=\"quiet splash resume=$swapuuid\"" \
    | tee /etc/default/grub.d/99_resume-from-hibernation.cfg \
          > /dev/null


# Enable hibernation using GUI dialogs.
# Adapted from: https://www.reddit.com/r/linuxmint/comments/93ta9u/
cat \
    > /etc/polkit-1/localauthority/50-local.d/com.ubuntu.enable-hibernate.pkla \
    <<EOF
[Re-enable hibernate by default]
Identity=unix-user:*
Action=org.freedesktop.upower.hibernate
ResultActive=yes

[Re-enable hibernate by default in logind]
Identity=unix-user:*
Action=org.freedesktop.login1.hibernate
ResultActive=yes
EOF


# Update GRUB configuration
update-grub
