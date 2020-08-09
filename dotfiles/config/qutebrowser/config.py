# config.py --- qutebrowser configuration

import dracula

### STFU, linters:

# From https://github.com/qutebrowser/qutebrowser/blob/master/doc/help/configuring.asciidoc#avoiding-flake8-errors

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103


### Bookmarklets:

# These are ports of my Firefox bookmarklets.

# For some reason this does not work with jseval:
# https://github.com/qutebrowser/qutebrowser/issues/221#issuecomment-434187331
c.aliases.update({'org-storelink': """open javascript:location.href='org-protocol://store-link?url='+encodeURIComponent(location.href)+'&title='+encodeURIComponent(document.title);"""})

c.aliases.update({'save-to-instapaper': """jseval javascript:function iprl5(){var d=document,z=d.createElement('scr'+'ipt'),b=d.body,l=d.location;try{if(!b)throw(0);d.title='(Saving...) '+d.title;z.setAttribute('src',l.protocol+'//www.instapaper.com/j/0YCsBKAiKza1?a=read-later&u='+encodeURIComponent(l.href)+'&t='+(new Date().getTime()));b.appendChild(z);}catch(e){alert('Please wait until the page has loaded.');}}iprl5();void(0)"""})


### Visuals:

# Load dracula theme:

dracula.blood(c)


### Load autoconfig:

# Load at bottom so that it can use definitions from config.py.

config.load_autoconfig()

