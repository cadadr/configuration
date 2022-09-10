#!/usr/bin/env bash
# load-screen-layout.bash --- load a saved screen layout

# bash strict mode
#set -euo pipefail
IFS=$'\n\t'

layout_dir="$HOME/.screenlayout"

declare -A layout_map

layout_scripts="$(ls $layout_dir/ | grep $HOSTNAME)"

for layout in $layout_scripts; do
    layout_id="$(basename -s .sh $layout | tr '-' ' ')"
    layout_map+=(["$layout_id"]="$layout")
done


for key in "${!layout_map[@]}"; do echo $key; done | dmenu | (
    read key
    exec $layout_dir/${layout_map[$key]}
)
