# config.py --- qutebrowser configuration

from qutebrowser.api import interceptor

### STFU, linters:

# From https://github.com/qutebrowser/qutebrowser/blob/master/doc/help/configuring.asciidoc#avoiding-flake8-errors

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103


### Load stuff:

config.source(config.configdir / "bookmarklets.py")
config.source(config.configdir / "keywords.py")


### Redirects:

def rewrite(request):
    # thank u, turkey!
    if request.request_url.host().endswith('imgur.com'):
        request.request_url.setHost('imgurp.com')

    try:
        request.redirect(request.request_url)
    except:  # noqa
        pass


interceptor.register(rewrite)


### Visuals:


### Load autoconfig:

# Load at bottom so that it can use definitions from config.py.

config.load_autoconfig()

