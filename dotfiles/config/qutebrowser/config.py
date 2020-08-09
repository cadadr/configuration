# config.py --- qutebrowser configuration

import dracula

### STFU, linters:

# From https://github.com/qutebrowser/qutebrowser/blob/master/doc/help/configuring.asciidoc#avoiding-flake8-errors

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103


### Bookmarklets:

config.source(config.configdir / "bookmarklets.py")


### Visuals:

# Load dracula theme:

dracula.blood(c)


### Load autoconfig:

# Load at bottom so that it can use definitions from config.py.

config.load_autoconfig()

