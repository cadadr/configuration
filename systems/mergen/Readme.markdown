# Setup recipe for `mergen`

After first boot, set a root password for when `sudo` breaks (and it
does, trust me)

    $ sudo su
    # passwd

and then enable source repositories using the "Software Sources" GUI
tool.  Preferably also install any updates Linux Mint may ask you to install
and then log out of the GUI session.  Switch to a virtual console
(`Ctrl+Alt+F1-6`) and start a root shell.

    $ sudo su
    #

The following commands can be run from inside vi(1) using `y$` for yanking
a command, and `:! Ctrl+R " <CR>` to paste it to the command line and
execute it.

Now it's time to go.


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

The file `mint.flatpak.install` contains a listing of Flatpaks to be
installed, which can be installed as follows:

    # xargs flatpak install -y < mint.flatpak.install

If you’ll use msmtp, the following command makes AppArmor quit its
stupid bullshit.  I’ve tried aliases to no avail.

    # aa-complain usr.bin.msmtp

Disable PulseAudio power management:

    # sed -i -E 's/^(load-module module-suspend-on-idle)$/# \1/' /etc/pulse/default.pa

Finally, we'll enable the GRUB menu with a sensible timeout and
configure resume from hibernation.

    # bash boot-setup.bash

Now it's time to go back to repo root and run `make setup` or
similar, while still on the virtual console.  It might be opportune
to run

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

Because this setup is rather new, it may require some time for it to
become more easily reproduced between installs.

---

![screen cap](/candy/scr-mergen.png)
