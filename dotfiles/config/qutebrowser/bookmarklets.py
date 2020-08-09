# bookmarklets.py --- ports of my Firefox bookmarklets

### STFU, linters:

# From https://github.com/qutebrowser/qutebrowser/blob/master/doc/help/configuring.asciidoc#avoiding-flake8-errors

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103


### Code:

# For some reason this does not work with jseval:
# https://github.com/qutebrowser/qutebrowser/issues/221#issuecomment-434187331
c.aliases.update({
    'org-storelink':
    "open javascript:location.href='org-protocol://store-link?"
    "url='+encodeURIComponent(location.href)"
    "+'&title='+encodeURIComponent(document.title);"
})


c.aliases.update({
    'org-capture':
    "open javascript:location.href='org-protocol://capture?"
    "template=N'+'&url='+encodeURIComponent(window.location.href)"
    "+'&title='+encodeURIComponent(document.title)"
    "+'&body='+encodeURIComponent(window.getSelection());"
})


c.aliases.update({
    'save-to-instapaper':
    """jseval javascript:function iprl5(){var d=document,z=d.createElement('scr'+'ipt'),b=d.body,l=d.location;try{if(!b)throw(0);d.title='(Saving...) '+d.title;z.setAttribute('src',l.protocol+'//www.instapaper.com/j/0YCsBKAiKza1?a=read-later&u='+encodeURIComponent(l.href)+'&t='+(new Date().getTime()));b.appendChild(z);}catch(e){alert('Please wait until the page has loaded.');}}iprl5();void(0)"""
})


c.aliases.update({
    'remove-fixed-elements': "jseval javascript:(function()%7B(function%20()%20%7Bvar%20i%2C%20elements%20%3D%20document.querySelectorAll('body%20*')%3Bfor%20(i%20%3D%200%3B%20i%20%3C%20elements.length%3B%20i%2B%2B)%20%7Bif%20(getComputedStyle(elements%5Bi%5D).position%20%3D%3D%3D%20'fixed')%20%7Belements%5Bi%5D.parentNode.removeChild(elements%5Bi%5D)%3B%7D%7D%7D)()%7D)()"
})


# Just adding this because it used to be there...
c.aliases.update({
    'post-to-hn': 'jseval javascript:window.location=%22http://news.ycombinator.com/submitlink?u=%22+encodeURIComponent(document.location)+%22&t=%22+encodeURIComponent(document.title)'
})


# Uncheck all checkboxes
c.aliases.update({
    'uncheckall':
    "javascript:document.querySelectorAll('input[type=checkbox]').forEach((elem) "
    '=> {elem.checked=false;})'
})


# IDK if this’d work with qutebrowser but just copying it over...
c.aliases.update({
    'connect-to-skewer':
    'javascript:(function(){var d=document;var '
    "s=d.createElement('script');s.src='http://localhost:8080/skewer';"
    "d.body.appendChild(s);})()"
})

