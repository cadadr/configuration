#!/usr/bin/env python3
# maildir2mbox.py --- convert maildir to mbox files.

import sys
import os
import mailbox
import email

if len(sys.argv) < 2:
    print("usage: maildir2mbox.py FOLDER... TARGET")
    exit(1)

target = sys.argv.pop()
folders = sys.argv[1:]

messages = []

for folder in folders:
    msgs = os.listdir(folder)
    idx = 0
    try: msgs.remove(".overview")
    except ValueError: pass
    for msg in msgs:
        idx += 1
        messages.append((os.path.join(folder, msg), idx))

messages.sort(key=lambda i: i[1])

mbox = mailbox.mbox(target)

for path, _ in messages:
    with open(path, mode="rb") as f:
        try:
            text = f.read()
            msg = email.message_from_bytes(text)
            mbox.add(msg)
        except Exception as e:
            print("Exception while processing %s: %s" % (path, e))
