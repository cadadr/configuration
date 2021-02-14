# Setup recipe for `kayra`

## Debian installation

The Debian [netinst](https://www.debian.org/distrib/netinst) image
should suffice for this setup.  Use the most current one.

Choose non-graphical installation, and go through the wizard.  Most of
the settings don’t really matter, but do create a user with the
username `g`, and set the hostname preferably to `kayra` and
domain to `local`.

On the «Software Selection» screen, select

- SSH server
- standard system utilities

This should be enough to support the rest of this setup script.
Continue with the next section after the installation completes and
you reboot into the newly installed operating system.

## `kayra` preparation

We use the newly installed Debian stable environment to set up a
testing system.

Open the system and log in as the superuser.

The following commands can be run from inside vi(1) using `y$` for yanking
a command, and `:! Ctrl+R " <CR>` to paste it to the command line and
execute it.

Now it's time to go.


The first step is to set up networking.  After the installation is
complete, we’ll use NetworkManager to handle this instead.

If this is an Ethernet based setup, no fiddling may be necessary, but
if a ping(1) fails, the interface may be down, so try the following
command to bring it up:

    # ip a
    ...
    # ip link set <eth_iface> up

If this is a WiFi based setup, we’ll need to run wpa\_supplicant(8)
and dhclient(8) manually to connect to the network and download the
necessary packages.  This is fairly simple, but wpa\_supplicant might
require some configuration if this is not a usual wireless network
with a WPA passphrase, in which case see wpa\_supplicant.conf(8).
Below is an example setup:

    # ip a
    ...
    # ip link set <wifi_iface> up
    # cat > wpa.conf <<EOF
    network={
    ssid="SSID"
    psk="passphrase"
    }
    EOF
    # wpa_supplicant -Dnl80211 -iwlp2s0 -c ./wpa.conf &
    ...
    # dhclient <wifi_iface>

At this point you can switch to using ssh(1) to remotely apply the
rest of these steps.  mdns is not enabled yet, so use `hostname -I` on
the target system to learn the IP address.  Do not forget to set
locales properly, otherwise chances are they will be inherited from
the parent shell of the ssh command.


    $ ssh g@192.168.1.75
    g@kayra:~$ export LC_ALL=en_GB.UTF-8 LANG=en_GB-UTF-8 LANGUAGE=en_GB-UTF-8
    g@kayra:~$ su
    Password:
    root@kayra:/home/g#

## `kayra` installation

Now with a system that can connect to the internet, it’s time to
proceed with the system installation.

First of all, we will install the few basic packages that will support
the rest of this guide, and clone this repository over to the target
system.

We will need to use git, make, m4, GNU awk, and Perl in order to
continue the setup.  Optionally, udisksctl from udisk2 may come
in handy.

    # apt-get install git make gawk perl m4 udisks2

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
    # echo igk-disk /dev/sdb2 none luks > /etc/crypttab
    # echo /dev/disk/by-label/igk-store /igk ext4 defaults 0 0 >> /etc/fstab

As it is now, this requires manually entering the decryption password
every boot.

Now, clone this repository.  It’s important to clone as the normal
user not as superuser:

    # exit
    $ git clone https://github.com/cadadr/configuration cf
    # su
    # cd cf/systems/kayra

(Note to self: it’s probably a better idea to clone from a repository
on the local network, or just use the working directory in the /igk
partition).

Install system-wide configuration files:

    # bash install-configs.bash

The file `debian.apt.install` contains a listing of Debian packages to
be installed, and the file `debian.apt.build-dep` contains a listing
of packages for which to fetch build dependencies.  The could be
installed by the following commands:

    # apt-get update
    # xargs apt-get install -y   < debian.apt.install
    # xargs apt-get build-dep -y < debian.apt.build-dep

After this, you might want to install documentation for the installed
packages.  There is a script for that:

    # sh ../../lib/install-doc-packages.sh

This will inspect the dpkg database and find out all the relevant
`-doc` packages, and install them.

Modify the user `g` to make it a member of some useful groups:

    # /sbin/usermod -a -G sudo,cdrom,floppy,audio,dip,video,plugdev,netdev,bluetooth,lpadmin,scanner -s /bin/bash g

Generate locales:

    # /sbin/locale-gen

At this point the system should be ready for the installation of my
dotfiles, for which see `../../Readme.markdown`.
