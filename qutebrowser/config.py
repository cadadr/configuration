# config.py --- qutebrowser configuration

from qutebrowser.api import interceptor
from qutebrowser.api import message

try:
    from qutebrowser.extensions.interceptors import ResourceType
except ImportError:
    pass

from qutebrowser import __version__ as qver

from datetime import timedelta, datetime
import os
import re

### STFU, linters:

# From https://github.com/qutebrowser/qutebrowser/blob/master/doc/help/configuring.asciidoc#avoiding-flake8-errors

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103

majv, minv, patch = map(int, qver.split("."))

# v2.x requires explicit suppression of loading autoconfig.yml. We
# will do that, but at the end of the config file, to give its
# settings precedence.
if majv >= 2:
    config.load_autoconfig(False)


### Load stuff:

config.source(config.configdir / "bookmarklets.py")
config.source(config.configdir / "keywords.py")


### Some settings:

c.url.searchengines['DEFAULT'] = \
    'https://duckduckgo.com/?q={}'


### Visuals:

# This is only set in custom sessions so no need to check if this is
# i3wm or anything.
colourscheme = os.getenv("GK_COLOUR_SCHEME_PREFERENCE")

if colourscheme and (colourscheme == "dark"):
    c.colors.webpage.preferred_color_scheme = "dark"

### Load autoconfig:

# Load at bottom so that it can use definitions from config.py.

config.load_autoconfig()

