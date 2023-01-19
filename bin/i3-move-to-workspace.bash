#!/usr/bin/env bash
# i3-move-to-workspace.bash --- fuzzy-match i3wm workspaces and move active container

# Adapted from https://cyberchaos.dev/kookie/nomicon/-/blob/main/\
#   infra/libkookie/modules/workstation/ui/i3/core/tools/i3-switch.nix

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

workspace="$(i3-msg -t get_workspaces \
                    | jq -r -M '.[] | .name' | sort -u \
                    | dmenu -p 'Move active window to workspace (match or new)')"

i3-msg move container to workspace $workspace
