# install.sh --- install base system

TZ=Europe/Istanbul
KEYS=uk

PART_root=sda7
PART_swap=sda5
PART_igk=sda6

say () {
	echo [$0 $(date)] $*
}

say Set timezone to $TZ...
timedatectl set-timezone $TZ

say Set keyboard layout to $KEYS...
loadkeys $KEYS

if [ -d /sys/firmware/efi/efivars ]; then
	say This is a UEFI system.
else
	say This is a BIOS system.
fi

say Set up WiFi...
wifi-menu

say Install installation tools...
pacman -Sy tmux parted

say Format root partition ($PART_root)...
mkfs.ext4 /dev/$PART_root

say Turn on swap on $PART_swap...
swapon $PART_swap

say Mount target partitions...
mount /dev/$PART_root /mnt

say Create igk mount point...
mkdir /mnt/igk

say Install base system...
pacstrap /mnt base base-devel multilib-devel linux-tools \
	xorg xorg-apps xorg-drivers xorg-fonts xorg-xinit xterm \
	wpa_supplicant dialog tmux parted emacs

say Generate fstab...
genfstab -U /mnt >> /mnt/etc/fstab
# igk partition
printf "/dev/sda6\t/igk\text4\trw,relatime\t0\t1\n" >> /mnt/etc/fstab

say Chroot into /mnt...
arch-chroot /mnt

say Set timezone...
ln -sf /usr/share/zoneinfo/$TZ /etc/localtime
hwclock --systohc

say Generate locales...
printf "en_GB.UTF-8 UTF-8\nen_US.UTF-8 UTF-8\ntr_TR.UTF-8 UTF-8" > /etc/locale.gen
locale-gen

say Configure system...
echo 'LANG=en_US.UTF-8' > /etc/locale.conf
echo 'KEYMAP=uk' > /etc/vconsole.conf
echo alpha > /etc/hostname
printf "127.0.0.1\tlocalhost\n::1\t\tlocalhost\n" >> /etc/hosts

say Generate initramfs...
mkinitcpio -p linux

say Set root password:
passwd
say Done.

say Install and configure bootloader...
say Download and install boot loader package...
pacman -Sy grub os-prober
say Install bootloader to /dev/sda...
grub-install --target=i386-pc /dev/sda
grub-mkconfig -o /boot/grub/grub.cfg

say Installation done, reboot now\!
