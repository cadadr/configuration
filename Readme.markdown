<img
 src="https://gitlab.com/cadadr/configuration/badges/master/build.svg"
 alt="Gitlab build status badge" />

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
-   My dotfiles (`dotfiles/`, `xdg-config/`)
-   My Debian GNU/Linux configuration (`systems/alpha/`)
-   My configuration for Raspberry Pi (`systems/pi/`)
-   And some other stuff.

I've re-created this repository from scratch, removing some secrets for
publishing. If you want to use any part of my configurations, you're
free to do so, but **do not** try to use it as is, it's complex and
personal, and some additional secret stuff is not included, so it won't
work reliably. Just cherry-pick what you want.

Lots of files created by other people are included in this repository,
most of the time verbatim. Any file that does not include a statement
for its licence terms is probably written by me, and I hereby put all of
them in public domain.

The sound file `candy/cowbell.wav` was adapted from the work of the
Wikimedia Commons user [I speak so
quietly](https://commons.wikimedia.org/w/index.php?title=User:I_speak_so_quietly),
see [here](https://commons.wikimedia.org/wiki/File:808,_cowbell.OGG).

Installation
============

`alpha` System setup
--------------------

In order to get this configuration running on a vanilla Debian stable
installation, run the following commands:

```
# apt-get install sudo make git python3 python3-distro
# make alpha-init
# locale-gen
# update-initramfs -u
```

This will trigger a series of shell scripts and make rules which will
initialise the system. It should not cause any problems to re-run this
command after fixing a failure that interrupts it running in order to
complete installation. Make sure an internet connection is available.

This recipe, among other things, will create a user `g` with uid 1993
and a group of similar identifiers, and assing the user certain groups.

`alpha` Post-install
--------------------

The following commands help complete the installation, regardless of the
system flavour:

```
$ make cron
$ update-desktop-database ~/.local/share/applications/
$ pip3 install -r requirements.txt
$ gem install bundler
$ bundle
$ gem rdoc --all
```
