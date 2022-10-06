Introduction
============

> Sic transit gloria mundi.

This repo contains the following:

- My dotfiles
- My Emacs configuration
- My Linux setups
- Supporting scripts and files
- Some other goodies

If you want to use any part of my configurations, feel free to do so,
but you probably don't want to use it as a whole, it's complex and
personal, and some additional secret stuff is not included, so it won't
work reliably. Just cherry-pick what you want.

Installation
============

System setup
------------

The configuration on this repo works with multiple OS / hardware
setups, which have their own set up instructions.  Follow the links
below for more details.

- [Linux mobile workstation](systems/sappho) (`sappho`) with Void
  Linux and i3 WM targeting ThinkPads, x230 in particular.

- [Linux desktop workstation](systems/diotima/) (`diotima`) with
  Linux Mint Cinnamon.

- [Raspberry Pi](systems/xanthippe/) (`xanthippe`) backup and print/scan
  server.

<table>
  <tr>
    <td>
      <img src="img/scr-sappho.png" alt="screenshot for sappho"/>
    </td>
    <td>
      <img src="img/scr-diotima.png" alt="screenshot for diotima"/>
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
`~/Sources/External/Emacs`, install it, and build and install the
Emacs config. The `emacs-up` rule will build from the git repo after
running `git pull`, whereas `emacs` will just do the build, without
updating the repo.

For installing [Qutebrowser](https://qutebrowser.org/). if necessary,
viz. [doc/Qutebrowser.markdown](doc/Qutebrowser.markdown).

For the user session to function as intended, you will need to log out and
log back in so that `$PATH` and similar are appropriately updated with the
emergent setup.

Licenses
========

Lots of files created by other people are included in this repository,
most of the time verbatim. Any file that does not include a statement
for its licence terms is probably written by me, and I hereby put all of
them in public domain.
