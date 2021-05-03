#!/usr/bin/env bash
# launch-session-servers.bash --- launch some servers

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

. $MYLIB/fns.sh

say launching servers...

say launch ruby documentation server...
ri --server=9394 2>$HOME/log/ri-server.log 1>$HOME/log/ri-server.log &

say launch user httpd...
lilserver.py 2>$HOME/log/lilserver.log 1>$HOME/log/lilserver.log &

say launch gemini to RSS/Atom gateway
start-g2ra.sh 2>$HOME/log/g2ra.log 1>$HOME/log/g2ra.log &

say launch dconf dumper...
while :; do
    sleep 5m
    /usr/bin/env sh $MYLIB/dconf-dump.sh \
     1>$HOME/log/dconf-dump.log 2>$HOME/log/dconf-dump.log
done &

say launch jupyterlab server...
( [ -d $HOME/Notes/JupyterLab ] \
      && cd $HOME/Notes/JupyterLab \
      && jupyter lab --no-browser --port=8889 \
                 1>$HOME/log/jupyterlab.log 2>$HOME/log/jupyterlab.log ) &

say done
