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

- My dotfiles (`dotfiles/`)
- My Emacs configuration
- My Linux configuration
- And some other stuff

I've re-created this repository from scratch, removing some secrets
for publishing. If you want to use any part of my configurations,
you're free to do so, but **do not** try to use it as a whole, it's
complex and personal, and some additional secret stuff is not
included, so it won't work reliably. Just cherry-pick what you want.

Installation
============

System setup
------------

The configuration on this repo works with multiple OS / hardware
setups, which have their own set up instructions.  Follow the links
below for more details.

- [Linux desktop workstation](systems/midori) (`midori`) based on Arch
  Linux and i3wm.

- [Linux desktop workstation](systems/umay/) (`umay`) with
  latest stable Linux Mint Debian Edition with Cinnamon desktop.

- [Experimental Linux desktop workstation](systems/guixtest/)
  (`guixtest`) with GuixSD and i3wm.

- [Linux desktop workstation](systems/kayra/) (`kayra`) with
  Debian and i3wm.

- [Linux laptop](systems/ulgen/) (`ulgen`) using the
  latest stable Linux Mint Cinnamon.

- [Raspberry Pi](systems/ayata/) (`ayata`) backup and print/scan
  server.

<table>
  <tr>
    <td>
      <img src="candy/scr-midori.png" alt="screenshot for midori"/>
    </td>
    <td>
      <img src="candy/scr-umay.png" alt="screenshot for umay"/>
    </td>
    <td>
      <img src="candy/scr-kayra.png" alt="screenshot for kayra"/>
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

    $ git clean -dfx       # in case the repo is dirty,
                           # commit beforehand if necessary.
    $ make setup build

Afterwards, the junk that `bin/invade` creates can be cleaned up
like this:

    $ find $HOME/ -name '*,invaded~' -exec rm -r \{\} \+

Now, you can log in to the graphical session.

GNU Emacs needs to be installed using the `build-emacs-master.sh` script,
as the Emacs setup included in this repository depends on new
features introuduced on the `master` branch of it.

    $ build-emacs-master.sh

If needed, a recent version of Qutebrowser can be installed via

    $ cd ~/co/External/github-qutebrowser-qutebrowser     # where a checkout usually
                                                          # is on my system
    $ python3 ./scripts/mkvenv.py --venv-dir ~/local/_qutebrowser
    $ cd -

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

The sound at `candy/radio.wav` is (C) 2012 Kijadzel, licensed under
the [CC-BY 3.0 License]. Obtained from
<https://freesound.org/s/170608/>.

The sound at `candy/bleep.wav` is (C) 2015 pan14, licensed under the
[CC0 License]. Obtained from
<https://freesound.org/people/pan14/sounds/263133/>.

The design at `candy/qute.svg`, the Qutebrowser logo, is (C) Florian
Bruhin.

The image at `candy/lagrange.png`, the Lagrange logo, is (C) Jaakko
Keränen.

[CC-BY 3.0 License]: http://creativecommons.org/licenses/by/3.0/
[CC0 License]: http://creativecommons.org/publicdomain/zero/1.0/
