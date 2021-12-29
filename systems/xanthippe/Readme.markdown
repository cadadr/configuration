# My Raspberry Pi setup

This repository contains my Raspberry Pi setup, which I use as a
networked printer/scanner and backup server for most of the time.

The setup is mainly done via the [Setup.org](./Setup.org) which is a
literate program to be used interactively with [Org
mode](https://orgmode.org) on [GNU Emacs](https://gnu.org/s/emacs). A
lot of detail is hidden in Github’s rendering of the file, so I
suggest you view it in Emacs instead.

This is quite fresh so not everything will work 100% of the time.  The
intended way to use Setup.org is to open it in Emacs and execute code
blocks one by one, preferably with an accompanying ssh session with an
interactive shell open into the Raspberry Pi.

The files under [etc/](./etc/) are installed to Raspberry Pi’s `/etc`
directory, so preferably do review and edit them to your liking before
installing them.

**BEWARE** that this is a personal configuration, so you’d ideally use
it as a basis to create your own config that fits your usage, rather
than to just take it wholesale and apply to your setup.

In any case tho feel free to make any sort of use you want to make out
of this.  Suggestions welcome in the issue tracker.
