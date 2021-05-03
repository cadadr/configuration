#!/bin/sh
cd $HOME/local/g2ra-deploy
./venv/bin/python3 -m gunicorn -b 127.0.0.1:1961 g2ra:app

