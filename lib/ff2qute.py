#!/usr/bin/env python3
# ff2qute.py --- converts firefox bookmarks to qutebrowser format

import sys; assert sys.version_info >= (3, 6, 0),\
    "this script requires at least python 3.6.0"

import sqlite3

from os import getenv
from pathlib import Path
from pprint import pprint

queries = {
    "bookmarks": "select url,moz_bookmarks.title from moz_places"
    " inner join moz_bookmarks on moz_places.id = moz_bookmarks.fk",
    "keywords": "select keyword,url,moz_places.title from moz_places"
    " inner join moz_keywords on moz_places.id = moz_keywords.place_id"
}


database = Path(getenv("DB") or "./places.sqlite")
outdir = Path(getenv("OUT") or "./")
if not outdir.exists():
    outdir.mkdir(parents=True, exists_ok=True)

connection = sqlite3.connect(database)
cursor = connection.cursor()

bookmarks = [
    {"url": x[0], "title": x[1] or f"[No title, url: {x[0]}]"}
    for x in cursor.execute(queries["bookmarks"])
]

keywords = [
    {"keyword": x[0], "url": x[1], "title": x[2]}
    for x in cursor.execute(queries["keywords"])
]

bookmarklets = [b for b in bookmarks if b['url'].startswith('javascript:')]

bookmarks_ = [b for b in bookmarks if b not in bookmarklets]

assert len(bookmarks) - len(bookmarklets) == len(bookmarks_)

bookmarks__ = [b for b in bookmarks_ if not b['url'].startswith('place:')]

# Write bookmarks
with open(outdir / "bookmarks", "w") as bf:
    for b in bookmarks__:
        print(f"{b['url']} {b['title']}", file=bf)

# Write bookmarklets
with open(outdir / "bookmarklets", "w") as blf:
    pprint(bookmarklets, stream=blf)

# Write keywords
with open(outdir / "keywords", "w") as kwf:
    pprint(keywords, stream=kwf)

