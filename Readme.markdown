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
- My Linux configuration
- And some other stuff.

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

- [Linux desktop workstation](systems/mergen/) (`mergen`) with
  latest stable Linux Mint Cinnamon.

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
      <img src="candy/scr-kayra.png" alt="screenshot for kayra"/>
    </td>
    <td>
      <img src="candy/scr-mergen.png" alt="screenshot for mergen"/>
    </td>
    <td>
      <img src="candy/scr-ulgen.png" alt="screenshot for ulgen"/>
    </td>
  </tr>
</table>

Post-install
------------

The following commands help complete the installation, regardless of the
system flavour:


```
$ make setup build
```

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


[CC-BY 3.0 License]: http://creativecommons.org/licenses/by/3.0/
[CC0 License]: http://creativecommons.org/publicdomain/zero/1.0/
