# `midori` installation

evaluate the following in vim so that ,y can evaluate commands (go to the
example line and type `0wly$: Ctrl+R "`

    :nmap ,y 02wy$:!<C-r>"

set up the kbd layout

    # loadkeys uk

enable NTP

    # timedatectl set-ntp true

format the root partition

    # mkfs.ext4 -L root /dev/disk/by-partlabel/root

mount necessary partitions

    # mount  /dev/disk/by-partlabel/root     /mnt
    # mkdir -p /mnt/boot
    # mount  /dev/disk/by-partlabel/umay-esp /mnt/boot
    # swapon /dev/disk/by-partlabel/swap

pacstrap

    # pacstrap /mnt base base-devel linux linux-firmware gvim man-db man-pages texinfo amd-ucode cryptsetup dhclient dhcpcd e2fsprogs grub efibootmgr os-prober iw iwd less lvm2 lynx openssh pv rsync tmux sudo wpa_supplicant wireless-regdb wireless_tools usbutils

generate temporary fstab

    # genfstab -U /mnt >> /mnt/etc/fstab

!!! SAVE THIS FILE IF YOU MADE ANY CHANGES !!!

chroot, vim this file from within the chroot

    # cp % /mnt/
    # arch-chroot /mnt vim -c 'nmap ,y 02wy$:\!<C-r>"' Installation.markdown

verify we're in chroot, /mnt should be empty

    # ls /mnt

time to set up time

    # ln -sf /usr/share/zoneinfo/Europe/Istanbul /etc/localtime
    # hwclock --systohc

locales

    # echo 'en_US.UTF-8 UTF-8'  > /etc/locale.gen
    # echo 'en_GB.UTF-8 UTF-8' >> /etc/locale.gen
    # echo 'tr_TR.UTF-8 UTF-8' >> /etc/locale.gen
    # echo 'LANG=en_GB.UTF-8'   > /etc/locale.conf
    # echo 'KEYMAP=uk'          > /etc/vconsole.conf
    # locale-gen

set up hostname

    # echo midori                                > /etc/hostname
    # echo '127.0.0.1	localhost'               > /etc/hosts
    # echo '::1		localhost'              >> /etc/hosts
    # echo '127.0.1.1	midori.local	midori' >> /etc/hosts
    # cat /etc/hosts

update fstab

    # cp /etc/fstab /etc/fstab.bak
    # mkdir -p /igk
    # echo '\# fstab'                                                             > /etc/fstab
    # echo '\# <file system>	<dir>	<type>		<options>	<dump>	<pass>' >> /etc/fstab
    # echo 'LABEL=root	/	ext4    rw,relatime	0	1'      >> /etc/fstab
    # echo 'PARTLABEL=igk	/igk	ext4    rw,relatime	0	1'      >> /etc/fstab
    # echo 'LABEL=umay-esp	/boot	vfat    rw,relatime,fmask=0022,dmask=0022,codepage=437,iocharset=ascii,shortname=mixed,utf8,errors=remount-ro	0 2' >> /etc/fstab
    # echo 'LABEL=swap	none	swap	defaults	0	0'      >> /etc/fstab
    # cat /etc/fstab

create initramfs

    # mkinitcpio -P

set root password

    # passwd

install grub

    # grub-install --target=x86_64-efi --efi-directory=/boot --bootloader-id=GRUB
    # echo 'GRUB_DISABLE_OS_PROBER=false' >> /etc/default/grub
    # grub-mkconfig -o /boot/grub/grub.cfg

well done! exit chroot, and reboot

let's add our shortcut back

    :nmap ,y 02wy$:!<C-r>"

now let's continue with setup

start network; first find the interface

    # ip a

change '7' accordingly

    # ip link set enp7s0 up
    # dhclient enp7s0

dhclient can report 'hostname: command not found', but it's not important. let's
test connectivity

    # ping -c 3 gkayaalp.com

install packages

    # sh ./install-packages.sh

optionally install AUR packages (Praat takes *ages* to compile).

    # bash ./install-aur.bash

allow sudoing for wheel members

    # EDITOR=vim visudo

create user

    # useradd -m -s /bin/bash -U -c cadadr g
    # usermod -a -G wheel g
    # passwd g

enable zeroconf networking

    # sed -i~bak '/^hosts:/s/\(resolve\)/mdns_minimal [NOTFOUND=return] \1/' /etc/nsswitch.conf
    # systemctl restart avahi-daemon

now reboot, and log in as the new user and proceed to [user
setup](../../Readme.markdowwn)

