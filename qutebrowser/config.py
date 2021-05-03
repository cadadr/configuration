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


### Redirects:
spacing_patterns = {}
last_visited = {}
spacing_minutes = {}


def add_spacing_pattern(_id, pattern, minutes):
    spacing_minutes[_id]  = minutes
    spacing_patterns[_id] = re.compile(pattern)

def do_redir(request):
    try:
        request.redirect(request.request_url)
    except Exception as e:
        message.warning(f"Error in config.py/do_redir(req): {e}")


# If within block period, redirect to block page, else, register.
def do_redirect_for_spacing(request, url, last, name):
    if last is not None:
        space = spacing_minutes[name]
        delta = timedelta(minutes=space)
        now = datetime.now()
        since = now - last
        if since < delta:
            frag = "{};{}".format(delta - since, url)
            newurl = f"http://localhost:1993/spaced.html#{frag}"
            request.request_url.setUrl(newurl)
            do_redir(request)
            return True
    return False


# Match against spaced browsing patterns, if match, register if first
# visit, ask to block otherwise.
def maybe_redirect_for_spacing(request):
    for name, pattern in spacing_patterns.items():
        url = request.request_url.toString()
        if re.match(pattern, url) is not None:
            last = last_visited.get(name)
            did_redirect = do_redirect_for_spacing(request, url, last, name)
            if did_redirect:
                return True
            else:
                last_visited[name] = datetime.now()
                break
    return False


def maybe_redirect_to_imgurp(request):
    # thank u, turkey!
    if request.request_url.host().endswith('imgur.com'):
        request.request_url.setHost('imgurp.com')
        do_redir(request)
        return True
    return False


def maybe_redirect_to_nitter(request):
    if request.request_url.host().endswith('twitter.com'):
        request.request_url.setHost('nitter.nixnet.services')
        do_redir(request)
        return True
    return False


# These functions should return True if they did a redirect.  The
# first match will be applied only.
redirect_fns = []

if majv >= 2:
    redirect_fns.append(maybe_redirect_to_imgurp)
    redirect_fns.append(maybe_redirect_to_nitter)
    # redirect_fns.append(maybe_redirect_for_spacing)


def redirect(request):
    if request.resource_type == ResourceType.main_frame:
        for fn in redirect_fns:
            if fn(request):
                break


interceptor.register(redirect)

add_spacing_pattern("mastodon", r"https?://toot\.cat/web/getting-started", 180)
add_spacing_pattern("reddit", r"https?://((www|old)\.)?reddit\.com/$", 300)
add_spacing_pattern("multireddits", r"https?://((www|old)\.)?reddit\.com/user/gkayaalp/m/.*", 300)

### Visuals:

# This is only set in custom sessions so no need to check if this is
# i3wm or anything.
colourscheme = os.getenv("GK_COLOUR_SCHEME_PREFERENCE")

if colourscheme and (colourscheme == "dark"):
    c.colors.webpage.preferred_color_scheme = "dark"

### Load autoconfig:

# Load at bottom so that it can use definitions from config.py.

config.load_autoconfig()

