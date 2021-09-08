Introduction
============

> Sic transit gloria mundi.

This repo contains the following:

- My dotfiles
- My Linux setups
- Supporting scripts and files
- Some other goodies

If you want to use any part of my configurations, feel free to do so,
but you probably don't want to use it as a whole, it's complex and
personal, and some additional secret stuff is not included, so it won't
work reliably. Just cherry-pick what you want.

**Note**: if you were looking for my GNU Emacs configuration, head to
[my emacs.d repo](https://github.com/cadadr/emacs.d).

Installation
============

System setup
------------

The configuration on this repo works with multiple OS / hardware
setups, which have their own set up instructions.  Follow the links
below for more details.

- [Linux desktop workstation](systems/kamaji/) (`kamaji`) with
  Linux Mint Cinnamon.

- [Linux laptop](systems/ulgen/) (`ulgen`) using the
  latest stable Linux Mint Cinnamon.

- [Raspberry Pi](systems/ayata/) (`ayata`) backup and print/scan
  server.

<table>
  <tr>
   <td>
      <img src="candy/scr-kamaji.png" alt="screenshot for kamaji"/>
    </td>
    <td>
      <img src="candy/scr-ulgen.png" alt="screenshot for ulgen"/>
    </td>
  </tr>
</table>

Post-install
------------

The following commands help complete the installation, regardless of the
system flavour.  Execute these **before logging in to the graphical
session**, in a virtual console shell:

    $ # install git configuration temporarily
    $ cp dotfiles/gitconfig.ini ~/.gitconfig
    $ # commit or stash any changes if necessary
    $ # remove -n (for dry run) and run
    $ git clean -dfxn
    $ make setup build

Afterwards, the junk that `bin/invade` creates can be cleaned up
like this:

    $ # remove 'echo' after checking output
    $ find $HOME/ -name '*,invaded~' -exec echo rm -r \{\} \+

Now, you can log in to the graphical session.

We can build and install Emacs now. Run

    $ make -C emacs.d emacs

or

    $ make -C emacs.d emacs-up

in order to build Emacs from a clone assumed to be found at
`~/co/External/Emacs`, install it, and build and install the Emacs
config. The `emacs-up` rule will build from the git repo after running
`git pull`, whereas `emacs` will just do the build, without updating
the repo.

If needed, a recent version of Qutebrowser can be installed via

    $ cd ~/co/External/github-qutebrowser-qutebrowser     # where a checkout usually
                                                          # is on my system
    $ python3 ./scripts/mkvenv.py --venv-dir ~/local/_qutebrowser
    $ cd -
    $ ~/local/_qutebrowser/bin/pip install readability-lxml

from inside a recent checkout.  The underscore is there in the path
name in order to tell `lib/profile/paths.sh` to skip adding this three
to the path, as otherwise it will shadow system’s Python 3
installation.

Qutebrowser is going to fail to keep login sessions because different
versions of WebEngine databases are incompatible
(viz. <https://github.com/qutebrowser/qutebrowser/issues/5847#issuecomment-718985559>),
so we need to delete the old database if it exists:

    $ rm -r $HOME/.local/share/qutebrowser/webengine

(`bin/launch-qutebrowser.bash` should deal with this automatically, in case
you followed the above steps to build and install Qutebrowser, instead of
installing it through a package manager.)


For the user session to function as intended, you will need to log out and
log back in so that `$PATH` and similar are appropriately updated with the
emergent setup.

Licenses
========

Lots of files created by other people are included in this repository,
most of the time verbatim. Any file that does not include a statement
for its licence terms is probably written by me, and I hereby put all of
them in public domain.
