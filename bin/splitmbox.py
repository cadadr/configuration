#!/usr/bin/env python3
# splitmbox.py --- split up an mbox

import mailbox
import sys
import os

if len(sys.argv) < 4:
    print("usage: splitmbox.py MEGABYTES MBOX NEWNAME")
    exit(1)

_, split_size, source, newname = sys.argv

split_size = int(split_size) * 1024 * 1024

mbox = mailbox.mbox(source)
idx = 1

# Find the next slice number.
files = list(filter(lambda fil: fil.startswith(newname + "-"), os.listdir('.')))
if files:
    files.sort()
    idx = int(files[-1].split("-")[1])


def newmbox():
    return "%s-%04d" % (newname, idx)


target = mailbox.mbox(newmbox())

for _, message in mbox.items():
    target.add(message)
    if os.stat(newmbox()).st_size >= split_size:
        target.flush()
        target.close()
        idx += 1
        target = mailbox.mbox(newmbox())

os.rename(newmbox(), newname)
