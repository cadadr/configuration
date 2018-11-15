#!/bin/sh
# 64clip.sh --- Base64-decode clipboard

xclip -out | base64 --ignore-garbage --decode 2>/dev/null \
    | xclip -selection clipboard
