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

The configuration on this repo works with multiple OS / hardware
setups, which have their own set up instructions.  Follow the links
below for more details.

- [GNU/Linux laptop](systems/ulgen/Setup.markdown) (`ulgen`) using the
  latest stable Linux Mint.

- [GNU/Linux desktop workstation](systems/mergen/) (`mergen`) (TBD).

- [Raspberry Pi](systems/mergen/) (`ayata`) backup and print/scan
  server.

Post-install
------------

The following commands help complete the installation, regardless of the
system flavour:


```
$ make setup
```
