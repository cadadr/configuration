#!/bin/sh
# gopher.sh --- open gopher linkx in lynx in xterm

exec xterm -e "lynx '$@'"
