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

- [Linux desktop workstation](systems/susulu/) (`susulu`) with
  KDE Neon.

- [Linux laptop](systems/ulgen/) (`ulgen`) using the
  latest stable Linux Mint Cinnamon.

- [Raspberry Pi](systems/ayata/) (`ayata`) backup and print/scan
  server.

<table>
  <tr>
   <td>
      <img src="candy/scr-susulu.png" alt="screenshot for susulu"/>
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

For the user session to function as intended, you will need to log out and
log back in so that `$PATH` and similar are appropriately updated with the
emergent setup.

Licenses
========

Lots of files created by other people are included in this repository,
most of the time verbatim. Any file that does not include a statement
for its licence terms is probably written by me, and I hereby put all of
them in public domain.
