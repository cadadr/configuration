#!/usr/bin/env bash
# zoom.bash --- start jailed zoom

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

mkdir -p $HOME/.jailed/zoom

firejail --apparmor --profile=$HOME/.config/firejail/zoom.local \
         --private=$HOME/.jailed/zoom zoom
