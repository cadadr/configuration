#!/bin/bash
# stage01.bash --- set up base system for sappho

# Resources this is based on:
#
# https://gitlab.com/cadadr/void-zfs-luks/-/blob/master/06-config-inside-chroot.sh
# file:///usr/share/doc/void/html/installation/guides/index.html from void-docs

set -euo pipefail

echo "===------------------------------------------------==="
echo "=   Hiya! This is Göktuğ's Void Linux Setup Script   ="
echo "=           Stage 01: set up base system             ="
echo "===------------------------------------------------==="
echo
echo === Beginning installation at $(date)...

if [ "$UID" != 0 ]; then
    echo root privileges required
    exit 1
fi

echo
echo === Update xbps...
echo
xbps-install -Su xbps

read -p '==> Device to set up on? ' destdisk

timezone=Europe/Istanbul
hostname=sappho
kbdlayout=uk
user=cadadr
groups=wheel,users,audio,video,cdrom,input,network

echo
echo === Partition ${destdisk}...
echo
sfdisk -q $destdisk <<EOF
label: gpt
device: $destdisk
unit: sectors
sector-size: 512

${destdisk}1 : start=2048, size=2097152, type=C12A7328-F81F-11D2-BA4B-00A0C93EC93B, name="${hostname}_efi"
${destdisk}2 : start=2099200, type=0FC63DAF-8483-4772-8E79-3D69D8477DE4, name="${hostname}_luks"
EOF

# let new disk setup settle
sleep 1
sync
sleep 1

echo
echo === Set up filesystems...
echo

cryptsetup luksFormat --type luks1 /dev/disk/by-partlabel/${hostname}_luks
echo
echo '==> Re-enter password to unlock new LUKS volume!'
echo
cryptsetup luksOpen /dev/disk/by-partlabel/${hostname}_luks ${hostname}_vg
vgcreate ${hostname}_vg /dev/mapper/${hostname}_vg
lvcreate --name ${hostname}_root -L 32G ${hostname}_vg
# (swap 24G, for hibernation w/ 16G ram)
lvcreate --name ${hostname}_swap -L 24G ${hostname}_vg
lvcreate --name ${hostname}_home -l 100%FREE ${hostname}_vg
mkfs.vfat /dev/disk/by-partlabel/${hostname}_efi
mkfs.ext4 -L root /dev/${hostname}_vg/${hostname}_root
mkfs.ext4 -L home /dev/${hostname}_vg/${hostname}_home
mkswap /dev/${hostname}_vg/${hostname}_swap

echo
echo === Prepare chroot...
echo

mount /dev/${hostname}_vg/${hostname}_root /mnt

for dir in dev proc sys run; do
    mkdir -p /mnt/$dir
    mount --rbind /$dir /mnt/$dir
    # fuck you racist fossbros for making me type
    # this command with this word
    mount --make-rslave /mnt/$dir
done

mkdir -p /mnt/home
mount /dev/${hostname}_vg/${hostname}_home /mnt/home
mkdir -p /mnt/boot/efi
mount /dev/disk/by-partlabel/${hostname}_efi /mnt/boot/efi

echo
echo === Install base packages...
echo

xbps-install -Sy -R https://alpha.de.repo.voidlinux.org/current \
             -r /mnt base-system cryptsetup grub-x86_64-efi lvm2 \
                     NetworkManager git borg wget w3m xmirror

echo
echo === Configure new system...
echo

echo $hostname > /mnt/etc/hostname
cat > /mnt/etc/locale.conf <<EOF
# locale.conf

LANG=en_GB.UTF-8
LC_COLLATE=C
EOF
echo 'en_US.UTF-8 UTF-8' >> /mnt/etc/default/libc-locales
echo 'en_GB.UTF-8 UTF-8' >> /mnt/etc/default/libc-locales
echo 'tr_TR.UTF-8 UTF-8' >> /mnt/etc/default/libc-locales

cat > /mnt/etc/fstab <<EOF
# fstab

# <file system>				<dir>		<type>	<options>		<dump>	<pass>
tmpfs					/tmp		tmpfs	defaults,nosuid,nodev	0	0
/dev/${hostname}_vg/${hostname}_root		/		ext4	defaults		0	0
/dev/${hostname}_vg/${hostname}_home		/home		ext4	defaults		0	0
/dev/${hostname}_vg/${hostname}_swap		swap		swap	defaults		0	0
/dev/disk/by-partlabel/${hostname}_efi	/boot/efi	vfat	defaults		0	0
EOF

echo 'GRUB_ENABLE_CRYPTODISK=y' >> /mnt/etc/default/grub

luksdev="$(blkid -o value -s UUID /dev/disk/by-partlabel/${hostname}_luks)"

sed -i -E "s/^(GRUB_CMDLINE_LINUX_DEFAULT=.*)\"$/\1 rd.lvm.vg=${hostname}_vg rd.luks.uuid=$luksdev\"/" \
    /mnt/etc/default/grub

echo
echo '==> Setting up volume key for the LUKS partition.'
echo '    Enter LUKS password when prompted!'
echo

dd bs=1 count=64 if=/dev/urandom of=/mnt/boot/volume.key
cryptsetup luksAddKey /dev/disk/by-partlabel/${hostname}_luks /mnt/boot/volume.key
chmod 000 /mnt/boot/volume.key
chmod -R g-rwx,o-rwx /mnt/boot

cat > /mnt/etc/crypttab <<EOF
# crypttab

# <name>	<device>				<password>		<options>
${hostname}_luks	/dev/disk/by-partlabel/${hostname}_luks	/boot/volume.key	luks
EOF

cat > /mnt/etc/dracut.conf.d/10-crypt.conf <<EOF
# 10-crypt.conf --- add encrypted volume key and info to initramfs

install_items+=" /boot/volume.key /etc/crypttab "
EOF

cat > /mnt/etc/rc.conf <<EOF
# rc.conf

FONT="ter-u14n"
HOSTNAME="$hostname"
HARDWARECLOCK="UTC"
TIMEZONE="$timezone"
KEYMAP="$kbdlayout"
EOF

mkdir -p /mnt/etc/polkit-1/rules.d/
cat > /mnt/etc/polkit-1/rules.d/50-org.freedesktop.NetworkManager.rules <<EOF
polkit.addRule(function(action, subject) {
  if (action.id.indexOf("org.freedesktop.NetworkManager.") == 0 && subject.isInGroup("network")) {
    return polkit.Result.YES;
  }
});
EOF

echo
echo === Finalise setup in chroot...
echo

# Finalise setup in chroot
cat > /mnt/payload.bash <<EOF
chown root:root /
chmod 755 /

echo Set root password
until passwd root; do
    echo passwords did not match for root, retry
done

useradd -m -s /bin/bash -U -G "$groups" "$user"
echo Set password for ${user}
until passwd ${user}; do
    echo passwords did not match for $user, retry
done

xbps-reconfigure -f glibc-locales

grub-install --target=x86_64-efi --efi-directory=/boot/efi \
    --bootloader-id="$hostname" $destdisk
xbps-reconfigure -fa

rm -fr /var/service/dhcpcd
rm -fr /var/service/wpa_supplicant
ln -s /etc/sv/NetworkManager /var/service
ln -s /etc/sv/dbus /var/service
EOF

echo
echo === Clean up...
echo

chroot /mnt bash payload.bash
rm /mnt/payload.bash
umount -R /mnt

echo
echo === Base system install 'complete!'
echo
echo Basic system set up, you may reboot. After reboot, run
echo visudo as root to allow users in the group wheel to run
echo commands as root, use nmtui or nmcli to get online, run
echo xbps-install -Su, and finally the stage 2 setup script.
echo
echo === Bye o/
echo
