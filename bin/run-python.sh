#!/bin/sh
# run-python.sh --- run python, with poetry if applicable

start="$PWD"

while true; do
    if [ -e pyproject.toml ]; then
        exec poetry run python $@
    elif [ / = "$PWD" ]; then
        cd $start
        exec ${RUN_PYTHON:-python3} $@
    else
        cd ..
    fi
done
