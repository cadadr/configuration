#!/bin/sh
# ydl-parallel.sh --- run youtube-dl in parallel.

# Either $1 is an input file, or stdin is used.  The input file is
# expected to contain one URL per line.

infile=${1:-/dev/stdin}
procs=${YDL_MAX_PROCS:-10}

ydl_path=$HOME/co/External/youtube-dl
export PYTHONPATH=$ydl_path

ydl_cmd="$ydl_path/bin/youtube-dl --no-progress --extract-audio"

xargs -L 1 -P $procs $ydl_cmd < $infile
