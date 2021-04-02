#!/usr/bin/env python3
# inirefmt.py --- clean up and sort .ini file

from collections import OrderedDict, UserDict
from configparser import ConfigParser
import sys


class SortedDict(UserDict):
    """Ordered dictionary that keeps itself sorted."""

    def __init__(self, **kwargs):
        super().__init__(**kwargs)
        self.data = OrderedDict(**kwargs)

    def __setitem__(self, key, value):
        super().__setitem__(key, value)
        self.data = OrderedDict(
            sorted(self.data.items(), key=lambda i: i[0])
        )


cp = ConfigParser(dict_type=SortedDict)

cs = sys.stdin.read()
cp.read_string(cs)

try:
    cp.write(sys.stdout)
except BrokenPipeError:
    pass

