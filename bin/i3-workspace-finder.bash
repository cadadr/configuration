#!/usr/bin/env bash
# i3-workspace-finder.bash --- fuzzy-match i3wm workspaces

# Adapted from https://cyberchaos.dev/kookie/nomicon/-/blob/main/\
#   infra/libkookie/modules/workstation/ui/i3/core/tools/i3-switch.nix

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

workspace="$(i3-msg -t get_workspaces \
                    | jq -r -M '.[] | .name' | sort -u \
                    | rofi -dmenu)"

i3-msg workspace $workspace
