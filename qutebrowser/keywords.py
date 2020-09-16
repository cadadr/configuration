# keywords.py --- ports of firefox keywords

### STFU, linters:

# From https://github.com/qutebrowser/qutebrowser/blob/master/doc/help/configuring.asciidoc#avoiding-flake8-errors

# pylint: disable=C0111
c = c  # noqa: F821 pylint: disable=E0602,C0103
config = config  # noqa: F821 pylint: disable=E0602,C0103


### Code:

c.url.searchengines['#debfile'] = \
    'https://packages.debian.org/search?searchon=contents&keywords={}&mode=filename&suite=unstable&arch=any'

c.url.searchengines['#hacettkutup'] = \
    'http://katalog.hacettepe.edu.tr/client/embedded.search/default/?ss=&ln=en_US&lm=ALL&q={}'

c.url.searchengines['#iep'] = \
    'https://www.google.com/cse?cx=001101905209118093242%3Arsrjvdp2op4&ie=UTF-8&q={}'

c.url.searchengines['#pt'] = \
    'https://dicionario.priberam.org/{}'

c.url.searchengines['#scholar'] = \
    'https://scholar.google.com.tr/scholar?hl=en&as_sdt=0%2C5&q={}'

c.url.searchengines['#sems'] = \
    'https://www.semanticscholar.org/search?=all&q={}'

c.url.searchengines['#tildes'] = \
    'https://duckduckgo.com/?q=%22{}%22+site%3Atildes.net&t=ffab&ia=web'

c.url.searchengines['#tr'] = \
    'http://sozluk.gov.tr/?q=&aranan=&gts=gts&aranan=&q={}'

c.url.searchengines['#wayback'] = \
    'https://web.archive.org/web/*/{}'

c.url.searchengines['#wc'] = \
    'https://www.worldcat.org/search?qt=worldcat_org_all&q={}'

c.url.searchengines['#aur'] = \
    'https://aur.archlinux.org/packages/?O=0&K={}'

c.url.searchengines['#lg'] = \
    'https://libgen.is/search.php?lg_topic=libgen&open=0&view=simple&res=25&phrase=1&column=def&req={}'

c.url.searchengines['#tren'] = \
    'https://www.wordreference.com/redirect/translation.aspx?dict=tren&w={}'

c.url.searchengines['#iten'] = \
    'https://www.wordreference.com/redirect/translation.aspx?dict=iten&w={}'

c.url.searchengines['#entr'] = \
    'https://www.wordreference.com/redirect/translation.aspx?dict=entr&w={}'

c.url.searchengines['#enit'] = \
    'https://www.wordreference.com/redirect/translation.aspx?dict=enit&w={}'

c.url.searchengines['#enpt'] = \
    'https://www.wordreference.com/redirect/translation.aspx?dict=enpt&w={}'

c.url.searchengines['#pten'] = \
    'https://www.wordreference.com/redirect/translation.aspx?dict=pten&w={}'

c.url.searchengines['#it'] = \
    'https://www.wordreference.com/redirect/translation.aspx?dict=itit&w={}'

c.url.searchengines['#engr'] = \
    'https://www.wordreference.com/redirect/translation.aspx?dict=engr&w={}'

c.url.searchengines['#gren'] = \
    'https://www.wordreference.com/redirect/translation.aspx?dict=gren&w={}'

c.url.searchengines['#deen'] = \
    'https://www.wordreference.com/redirect/translation.aspx?dict=deen&w={}'

c.url.searchengines['#ende'] = \
    'https://www.wordreference.com/redirect/translation.aspx?dict=ende&w={}'

c.url.searchengines['#maps'] = \
    'https://www.google.com/maps/@?q={}'

c.url.searchengines['#wikt'] = \
    'https://en.wiktionary.org/wiki/Special:Search?search={}'

c.url.searchengines['#de'] = \
    'https://de.thefreedictionary.com/_/search.aspx?tab=262144&SearchBy=0&TFDBy=0&Word={}'

c.url.searchengines['#qref'] = \
    'https://quickref.dev/search?type=all&q={}'

c.url.searchengines['#hoog'] = \
    'https://hoogle.haskell.org/?scope=set%3Astackage&hoogle={}'

c.url.searchengines['#define'] = \
    'https://www.collinsdictionary.com/search/?dictCode=english&q={}'

c.url.searchengines['#urban'] = \
    'https://www.urbandictionary.com/define.php?term={}'

c.url.searchengines['#goog'] = \
    'https://www.google.com/search?sxsrf=ALeKk022kZ8ujZnTnIPTzoTOZqdBuETOUw%3A1590809698043&source=hp&ei=YtTRXr8ViuFSyaKQkA8&q={}'

c.url.searchengines['#emoji'] = \
    'https://emojipedia.org/search/?q={}'

c.url.searchengines['#rseek'] = \
    'https://rseek.org/?search={}'

c.url.searchengines['#cran'] = \
    'https://www.r-pkg.org/search.html?q={}'

c.url.searchengines['/r'] = \
    'https://reddit.com/r/{}'

youtube = "https://www.youtube.com/results?search_query={}"

c.url.searchengines['#yt'] = youtube
c.url.searchengines['#youtube'] = youtube
