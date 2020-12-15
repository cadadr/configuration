# Setup recipe for `ulgen`

First of all, install `ulgen`’s specific dotfiles:

    $ invade -v $HOME

The file `linuxmint.packages.apt` contains a listing of Debian-formatted
packages to be installed, in a format usable with Linux Mint's Backup
Tool. However, a command like the one below should be able to install
all of those:

    # apt-get install $(grep install$ linuxmint.packages.apt | awk '{print $1}')

but I haven’t tested this yet.  This should work on any Ubuntu-based
distribution.

After this, you might want to install documentation for the installed
packages.  There is a script for that:

    # sh ../../lib/install-doc-packages.sh

This will inspect the dpkg database and find out all the relevant
`-doc` packages, and install them.

I've also recently added a list of Flatpaks to be installed in
`linuxmint.packages.flatpak`.  These can be installed using a command
like:

    $ flatpak install flathub $(grep -v ^# linuxmint.packages.flatpak)

On my system I have to follow this up with the below commands to make
the Zotero Flatpak work:

    $ flatpak override --user --filesystem=$(readlink ~/.zotero) org.zotero.Zotero
    $ flatpak override --user --filesystem=$(readlink ~/.zotarc) org.zotero.Zotero

because those symlinks point to directories outside my `$HOME`
directory.

In order to finalise package installation before setting up the
dotfiles, the build dependencies for GUI Emacs and Python 3 are
necessary:

    # apt-get build-dep python3 emacs-gtk emacs-pdf-tools

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

![screen cap](/candy/scr.png)
