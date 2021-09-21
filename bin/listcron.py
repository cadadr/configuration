#!/usr/bin/env python3
# listcron.py --- list upcoming cron jobs with human-readable scheduled times

# Adapted from: https://unix.stackexchange.com/questions/181347/time-remaining-for-the-next-run

import subprocess
import re

from croniter import croniter
from datetime import datetime

lines = subprocess.Popen("crontab -l", shell=True, stdout=subprocess.PIPE).stdout.readlines()

print("Upcoming cron jobs:\n")
print("min\thr\tday\tmonth\tweek\tdatetime\t\tcommand to be executed")
print("------------------------------------------------------------------------------------------")

for line in lines:
    l = line.decode('utf-8').strip()
    if re.match(r"^\*|[0-9]{1,2}", l):
        parts = l.split(maxsplit=5)
        patn = "\t".join(parts[:5])
        i = croniter(patn, datetime.now())
        print(f"{patn}\t{i.get_next(datetime)}\t{parts[-1]}")


