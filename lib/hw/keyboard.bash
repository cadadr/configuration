#!/usr/bin/env bash
# keyboard.bash --- default keyboard settings

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

setxkbmap gb
setxkbmap -option  ctrl:nocaps
