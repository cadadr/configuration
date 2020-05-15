#!/usr/bin/env bash
# gk-open-project.bash --- open project using emacsclient, rofi & gk-open-project

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

p="$(rofi -lines 0 1 -dmenu -p 'Project path')"

exec emacsclient -eval "(gk-open-project \"$p\")"
