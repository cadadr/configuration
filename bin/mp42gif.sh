#!/bin/sh

set -e

if [ -z "$1" ]; then
    echo "usage: $0 file"; exit 1;
fi

if [ ! -f "$1" ]; then
    echo "no such file: $1";
fi

tmp=$(mktemp -d -t mp42gifconv.XXXXXXXXXXXXX)

ffmpeg -loglevel fatal -i $1 -r 2 $tmp/%010d.jpg

out=$(dirname $1)/$(basename $1 .mp4).gif

convert $tmp/*.jpg $out

echo Wrote $out

rm -rf $tmp
