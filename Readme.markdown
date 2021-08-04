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

- [Linux desktop workstation](systems/itugen/) (`itugen`) with
  latest stable Debian with MATE desktop.

- [Linux laptop](systems/ulgen/) (`ulgen`) using the
  latest stable Linux Mint Cinnamon.

- [Raspberry Pi](systems/ayata/) (`ayata`) backup and print/scan
  server.

<table>
  <tr>
   <td>
      <img src="candy/scr-itugen.png" alt="screenshot for itugen"/>
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
