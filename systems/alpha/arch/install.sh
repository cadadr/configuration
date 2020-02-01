# arch-install.sh --- install archlinux

### Variables:
echo == Setup...
VG=alpha
# Target tree root:
TR=/mnt
part_efi=/dev/sda1
part_boot=/dev/sda2
part_lvm=/dev/sda3
part_root=/dev/mapper/$VG-root
part_home=/dev/mapper/$VG-home
part_swap=/dev/mapper/$VG-swap


### Prelude:
echo == Keyboard settings...
loadkeys uk
# loadkeys /usr/share/kbd/keymaps/i386/include/ctrl.map.gz # caps as ctrl

echo == Set timezone...
timedatectl set-ntp true

export TZ=Europe/Istanbul


### Partition setup:
echo == Setup partitions...
setup_lvm () {
    memsiz=$(grep MemTotal /proc/meminfo | awk '{print $2}')
    memgb=$(units --compact "$memsiz kilobytes" gigabytes | head -1 | awk '{print int($1+1)}')
    swapsiz=$(( 2 * $memgb ))

    vgcreate $VG $part_lvm
    lvcreate -L 32G -n root $VG
    lvcreate -L 32G -n home $VG
    lvcreate -L ${swapsiz}G -n swap $VG
    lvcreate -l 100%FREE -n igk $VG
}

echo == Format filesystems...
mkfs.fat -F32 -n EFISYS $part_efi
mkfs.ext4 -L linux-boot $part_boot
mkfs.ext4 -L linux-root $part_root
mkfs.xfs -L linux-home $part_home
mkswap -L linux-swap $part_swap


### Mount filesystems:
echo == Mount filesystems...
swapon $part_swap
mount $part_root $TR
mkdir $TR/home $TR/boot
mount $part_home $TR/home
mount $part_boot $TR/boot


### Install:
echo == Install base...
pacstrap $TR base linux linux-firmware vim tmux man-db man-pages texinfo \
         wpa_supplicant xfsprogs btrfs-progs grub grub-btrfs snapper     \
         git rcs bash-completion ca-certificates efibootmgr os-prober    \
         networkmanager gvfs lvm2 dhclient pulseaudio lightdm            \
         xorg-xserver                                                    \
         xfce4 xfce4-goodies ffmpegthumbnailer poppler-glib libgsf       \
         libopenraw freetype2 xarchiver pavucontrol                      \
         python guile xz lzop lz4 bzip2 gawk ruby perl gzip gcc          \
         python-pip

echo == Generate fstab...
genfstab -U $TR >> $TR/etc/fstab


#### Chroot:
echo == Go into arch-chroot...
arch-chroot $TR

echo == Set timezone...
ln -sf /usr/share/zoneinfo/$TZ /etc/localtime
hwclock --systohc

echo == Create initial configuration files...
cat > /etc/locale.gen <<END
# locale.gen(5)

en_GB.UTF-8 UTF-8
en_US.UTF-8 UTF-8
tr_TR.UTF-8 UTF-8
END

locale-gen

cat > /etc/locale.conf <<END
# locale.conf(5)

LANG=en_GB.UTF-8
END

cat > /etc/vconsole.conf <<END
# vconsole.conf

KEYMAP=uk
END

echo alpha > /etc/hostname

cat > /etc/hosts <<END
# hosts(5)

127.0.0.1	localhost
::1		localhost
127.0.0.1	alpha.local alpha
END


echo == Mkinitcpio

patch /etc/mkinitcpio.conf < 01_etc-mkinitcpio.conf.patch

mkinitcpio -P

echo == Install bootloader
# FIX: uefi?
grub-install --target=i386-pc /dev/sda
grub-mkconfig -o /boot/grub/grub.cfg


### Completed!:
echo
echo
echo
echo Installation complete\!
echo
echo Please run \`passwd\' in order to set root password.
echo
echo After that, you can safely reboot into your fresh system.
echo
