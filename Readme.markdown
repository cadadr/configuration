![docker-build](https://github.com/cadadr/configuration/workflows/docker-build/badge.svg)

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

-   My GNU Emacs configuration tree (`emacs.d/`)
-   My dotfiles (`dotfiles/`)
-   My GNU/Linux configuration
-   And some other stuff.

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

System setup
------------

As of recent I’ve moved to Linux Mint to have a simpler config setup
(after commit a68595e).  At the time of writing this there is not much
that is specific to Linux Mint, but I make use of the Backup Tool to
install all my packages.  In order to use it, run the app, click the
Restore under Software Selection, and select `linuxmint.packages.list`
file.  It’ll take a while to install all the packages.

If you want to use this setup with Ubuntu, first check `invasion` and
then `dotfiles/config` to see if there is anything that you’d want to
disable or remove.  Then, a command like the one below should be able
to install all the packages from `linuxmint.packages.list`:

    # apt-get install $(awk '{print $1}' < linuxmint.packages.list)

but I haven’t tested this yet, there may be some packages that are not
available on Ubuntu, or whatever Ubuntu-based distro you’re using.

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
$ sudo sh lib/guix-install.sh
$ guix pull && guix package -u
$ guix package -m lib/guix/manifest.scm
```

---

![screen cap](candy/scr.png)
