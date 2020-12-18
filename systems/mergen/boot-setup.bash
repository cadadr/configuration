#!/usr/bin/env bash
# boot-setup.bash --- enable hibernation and GRUB menu under ubuntu et al.

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

# Check for superuser priviledges
if [ $UID -ne 0 ]; then
    echo This script requires superuser priviledges\!
    exit 2
fi


# Enable boot menu with some timeout
printf "GRUB_TIMEOUT_STYLE=menu\nGRUB_TIMEOUT=30\n" \
    | tee /etc/default/grub.d/99_showmenu.cfg > /dev/null


# Enable resuming from hibernation
swapuuid="$(grep ^UUID=.*swap /etc/fstab | cut -d ' ' -f 1)"
echo "GRUB_CMDLINE_LINUX_DEFAULT=\"quiet splash resume=$swapuuid\"" \
    | tee /etc/default/grub.d/99_resume-from-hibernation.cfg \
          > /dev/null


# Enable hibernation using GUI dialogs
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
