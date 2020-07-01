Introduction
============

> Sic transit gloria mundi.

Welcome to Göktuğ's Configuration, a work of artisanal love and care
aimed at making the personal computer use a joyful activity, instead of
endless agony and trade-offs in the era of inconfigurable apps designed
with biblical unquestionable dogmas imposed on the users by the people
that go by the D-word as their job title. My computer is my atelier, not
an exhibition, so I demand that I'm able to touch, move and modify
anything in any way.

This repo contains the following:

- My GNU Emacs configuration tree (`emacs.d/`)
- My dotfiles (`dotfiles/`)
- My GNU/Linux configuration
- And some other stuff.

I've re-created this repository from scratch, removing some secrets
for publishing. If you want to use any part of my configurations,
you're free to do so, but **do not** try to use it as a whole, it's
complex and personal, and some additional secret stuff is not
included, so it won't work reliably. Just cherry-pick what you want.

Lots of files created by other people are included in this repository,
most of the time verbatim. Any file that does not include a statement
for its licence terms is probably written by me, and I hereby put all of
them in public domain.

Installation
============

The first thing after cloning this repo is to initialise the submodules

    $ git submodule update --init


System setup
------------

The file `linuxmint.packages.apt` contains a listing of Debian-formatted
packages to be installed, in a format usable with Linux Mint's Backup
Tool. However, a command like the one below should be able to install
all of those:

    # apt-get install $(grep install$ linuxmint.packages.apt | awk '{print $1}')

but I haven’t tested this yet.  This should work on any Ubuntu-based
distribution.

After this, you might want to install documentation for the installed
packages.  There is a script for that:

    # sh lib/install-doc-packages.sh

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

---

If you want to reproduce this setup on a Debian based OS, then it’s a
bit more involved given there are some differing package names.  You
might want to go through the package list and find those
discrepancies, fix them, and run the above command, possibly with a
different file name.

Because this setup is rather new, it may require some time for it to
become more easily reproduced between installs.

Post-install
------------

The following commands help complete the installation, regardless of the
system flavour:


```
$ make setup
```

The following commands set up Guix, an experimental addition.

```
$ sudo sh lib/guix/guix-install.sh
$ sudo systemctl enable guix-daemon
$ guix pull && guix package -u
$ guix package -m lib/guix/manifest.scm
```

---

![screen cap](candy/scr.png)
