# config.py --- qutebrowser configuration

from qutebrowser.api import interceptor
from qutebrowser import __version__ as qver
from dracula import blood

from datetime import timedelta, datetime
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


### Theme:
blood(c)


### Some settings:

c.url.searchengines['DEFAULT'] = \
    'https://duckduckgo.com/?q={}'


### Redirects:
spacing_patterns = {}
spacing_patterns["mastodon"] = \
    re.compile(r"https?://mastodon\.sdf\.org/web/getting-started")
spacing_patterns["reddit"] = \
    re.compile(r"https?://((www|old)\.)?reddit\.com/")

last_visited = {}
spacing_minutes = {}
spacing_minutes["mastodon"] = 180
spacing_minutes["reddit"]   = 300


def rewrite(request):
    # thank u, turkey!
    if request.request_url.host().endswith('imgur.com'):
        request.request_url.setHost('imgurp.com')

    for name, pattern in spacing_patterns.items():
        url = request.request_url.toString()
        if re.match(pattern, url) is not None:
            last = last_visited.get(name)
            if last is not None:
                space = spacing_minutes[name]
                delta = timedelta(minutes=space)
                now   = datetime.now()
                since = now - last
                if since < delta:
                    frag = "{};{}".format(delta - since, url)
                    newurl = f"http://localhost:1993/spaced.html#{frag}"
                    request.request_url.setUrl(newurl)
                else:       # clear if block time elapsed
                    last_visited[name] = None

            else:
                last_visited[name] = datetime.now()


    try:
        request.redirect(request.request_url)
    except:  # noqa
        pass


interceptor.register(rewrite)


### Visuals:


### Load autoconfig:

# Load at bottom so that it can use definitions from config.py.

config.load_autoconfig()

