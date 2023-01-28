#!/usr/bin/env bash
# tex.bash --- install (La)TeX shite

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

xbps-install -y \
             texlive-full \
             texlive-fontsextra
