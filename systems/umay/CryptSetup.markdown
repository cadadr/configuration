# Encrypted drive setup

In my personal setup, I keep my files in a dedicated ext4 partition
encrypted with LUKS.  In order to mount it, first unlock the encrypted
device:

    # lsblk
    # udisksctl unlock -b /dev/...

The output from this command lists the path to the newly created
block device file that can be used to mount the partition:

    # mkdir /igk
    # mount /dev/dm-0 /igk

The following commands allow to mount this partition at boot:

    # blkid
    # echo igk-disk /dev/sdb2 none luks >> /etc/crypttab
    # echo /dev/disk/by-label/igk-store /igk ext4 defaults 0 0 >> /etc/fstab

As it is now, this requires manually entering the decryption password
every boot.

