# Setup recipe for `mergen`

After first boot, set a root password for when `sudo` breaks (and it
does, trust me)

    $ sudo su
    # passwd

and then enable source repositories using the "Software Sources" GUI
tool.  Preferably also install any updates Linux Mint may ask you to install
and then start a root shell:

    $ sudo su
    #

The following commands can be run from inside vi(1)/vim(1) using `y$`
for yanking a command, and `:! Ctrl+R " <CR>` to paste it to the
command line and execute it.

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


Now it's time to get going with the installation process.


The file `mint.apt.install` contains a listing of Debian packages to be
installed, and the file `mint.apt.build-dep` contains a listing of
packages for which to fetch build dependencies.  The could be
installed by the following commands:

    # xargs apt-get install -y   < mint.apt.install
    # xargs apt-get build-dep -y < mint.apt.build-dep

After this, you might want to install documentation for the installed
packages.  There is a script for that:

    # sh ../../lib/install-doc-packages.sh

This will inspect the dpkg database and find out all the relevant
`-doc` packages, and install them.

If you’ll use msmtp, the following command makes AppArmor quit its
stupid bullshit.  I’ve tried aliases to no avail.

    # aa-complain usr.bin.msmtp

Disable PulseAudio power management (insert `\\` before `#` in the
command below to escape it when calling from vi/vim command line):

    # sed -i -E 's/^(load-module module-suspend-on-idle)$/# \1/' /etc/pulse/default.pa

Finally for the system setup, we'll enable the GRUB menu with a
sensible timeout and configure resume from hibernation.

    # bash boot-setup.bash

The last step is to install flatpaks, which should be done after
exiting the root shell, i.e. without superuser privileges.

The file `mint.flatpak.install` contains a listing of Flatpaks to be
installed, which can be installed as follows:

    $ xargs flatpak install -y < mint.flatpak.install

The following command will allow the Lagrance flatpak to gain access
to the external file I keep its bookmarks at:

    $ flatpak override fi.skyjake.Lagrange --user --filesystem=$HOME/Notes/Lagrange-bookmarks.txt 

Now it's time to go back to repo root and run `make setup` or
similar. **Log out of the graphical session for this and switch
to a virtual console.**  It might be opportune to run

    $ git clean -dfx

before that step to rid of useless files, but **make sure to** commit
or stash changes you made during this installation process before
that, in order to not lose your modifications.

---

If you want to reproduce this setup on a Debian based OS, then it’s a
bit more involved given there are some differing package names.  You
might want to go through the package list and find those
discrepancies, fix them, and run the above command, possibly with a
different file name.

---

![screen cap](/candy/scr-mergen.png)
