# Setup recipe for `kayra`

## elementaryOS installation

Choose graphical installation, and go through the wizard.  Most of
the settings don’t really matter, but do create a user with the
username `g`, and set the hostname preferably to `talay`.

This should be enough to support the rest of this setup script.
Continue with the next section after the installation completes and
you reboot into the newly installed operating system.

After first boot, log in to the graphical session, and make sure
to enable source repositories.

<!-- XXX: is this necessary? -->
With that done, now log out from the graphical session, and switch
to a Linux virtual console via `Ctrl+Alt+F1--6' in order to carry
out the following proedure.

## `talay` installation

Now with a system that can connect to the internet, it’s time to
proceed with the system installation.

First of all, we will install the few basic packages that will support
the rest of this guide, and clone this repository over to the target
system.

We will need to use git, make, m4, GNU awk, and Perl in order to
continue the setup.  Optionally, udisksctl from udisk2 may come
in handy.

    $ sudo apt-get install git make gawk perl m4 udisks2 flatpak

The file `apt.install` contains a listing of Debian packages to
be installed, and the file `apt.build-dep` contains a listing
of packages for which to fetch build dependencies.  The could be
installed by the following commands:

    $ sudo apt-get update
    $ sudo xargs apt-get install -y   < apt.install
    $ sudo xargs apt-get build-dep -y < apt.build-dep

It might be the case that errors are reported for some packages, most
probably because of a clash with the config files we installed into
`etc`.  Normally, `dpkg` prompts for these clashes, but because output
will not be a TTY here, those prompts will be skipped.  If you find
yourself in this situation, run

    $ sudo apt-get install -f

in order to reconfigure these packages interactively.

After this, you might want to install documentation for the installed
packages.  There is a script for that:

    $ sudo sh ../../lib/install-doc-packages.sh

This will inspect the dpkg database and find out all the relevant
`-doc` packages, and install them.

A recent version of Qutebrowser can be installed via

    $ python3 ./scripts/mkvenv.py --venv-dir ~/local/_qutebrowser

from inside a recent checkout.  The underscore is there in the path
name in order to tell `lib/profile/paths.sh` to skip adding this three
to the path, as otherwise it will shadow system’s Python 3
installation.

Qutebrowser is going to fail to keep login sessions because different
versions of WebEngine databases are incompatible
(viz. <https://github.com/qutebrowser/qutebrowser/issues/5847#issuecomment-718985559>),
so we need to delete the old database if it exists:

    $ rm -r $HOME/.local/share/qutebrowser/webengine

Now it's time to go back to repo root and run `make setup` or similar
(see `../../Readme.markdown`). **Log out of the graphical session for
this and switch to a virtual console.** It might be opportune to run

    $ git clean -dfx

before that step to rid of useless files, but **make sure to** commit
or stash changes you made during this installation process before
that, in order to not lose your modifications.


At this point the system should be ready for the installation of my
dotfiles, for which see [the relevant Readme](../../Readme.markdown).

---

![screen cap](/candy/scr-talay.png)
