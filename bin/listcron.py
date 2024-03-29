#!/usr/bin/env python3
# listcron.py --- list upcoming cron jobs with human-readable scheduled times

# Adapted from: https://unix.stackexchange.com/questions/181347/time-remaining-for-the-next-run

import subprocess
import sys
import re

from croniter import croniter
from datetime import datetime

verbose = False

def usage(exit_code=0):
    print("listcron.py: usage: listcron.py [-vh]")
    exit(exit_code)

try:
    arg = sys.argv[1]
    if arg == '-v':
        verbose = True
    elif arg == '-h':
        usage()
    else:
        usage(1)
except IndexError:
    pass


lines = subprocess.Popen(
    "crontab -l", shell=True, stdout=subprocess.PIPE
).stdout.readlines()

print("Upcoming cron jobs:\n")

if verbose:
    print("min\thr\tday\tmonth\tweek\tdatetime\t\tcommand to be executed")
    print("-" * 90)
else:
    print("datetime\t\tcommand to be executed")
    print("-" * 46)

for line in lines:
    l = line.decode('utf-8').strip()
    if re.match(r"^\*|[0-9]{1,2}", l):
        parts = l.split(maxsplit=5)
        patn = "\t".join(parts[:5])
        i = croniter(patn, datetime.now())
        patn_ = f"{patn+'	' if verbose else ''}"
        time  = f"{i.get_next(datetime)}"
        cmd   = f"{parts[-1]}"
        print(f"{patn_}{time}\t{cmd}")


