# Encrypted drive setup

In my personal setup, I keep my files in a dedicated ext4 partition
encrypted with LUKS.

## Creating the device

Create an empty, unformatted partition with fdisk, GParted, etc., and
format it with cryptsetup as follows:

    # cryptsetup luksFormat /dev/...

Then, unlock the freshly created LUKS partition:

    # sudo udisksctl unlock -b /dev/...
    Unlocked /dev/... as /dev/dm-2.

Now, create a Linux partition inside it:

    # mkfs.ext4 -L igkdata /dev/dm-2

Mount the filesystem, and change the owner:

    # mount /dev/dm-2 /mnt
    # chown -R g:g /mnt

## Mounting and setting up automount

In order to mount it, first unlock the encrypted
device:

    # lsblk
    # udisksctl unlock -b /dev/...

The output from this command lists the path to the newly created
block device file that can be used to mount the partition:

    # mkdir /igk
    # mount /dev/dm-0 /igk

The following commands allow to mount this partition at boot:

    # blkid
    # echo igk-disk /dev/... none luks >> /etc/crypttab
    # echo /dev/disk/by-label/igkdata /igk ext4 defaults 0 0 >> /etc/fstab

As it is now, this requires manually entering the decryption password
every boot.

