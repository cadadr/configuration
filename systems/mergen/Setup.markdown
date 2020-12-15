# Setup recipe for `mergen`

The file `pop.apt.install` contains a listing of Debian packages to be
installed, and the file `pop.apt.build-dep` contains a listing of
packages for which to fetch build dependencies.  The could be
installed by the following commands:

    # xargs apt-get install -y   < pop.apt.install
    # xargs apt-get build-dep -y < pop.apt.build-dep

After this, you might want to install documentation for the installed
packages.  There is a script for that:

    # sh ../../lib/install-doc-packages.sh

This will inspect the dpkg database and find out all the relevant
`-doc` packages, and install them.

The file `pop.flatpak.install` contains a listing of Flatpaks to be
installed, which can be installed as follows:

    # xargs flatpak install -y < pop.flatpak.install

If you’ll use msmtp, the following command makes AppArmor quit its
stupid bullshit.  I’ve tried aliases to no avail.

    # sudo aa-complain usr.bin.msmtp

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
