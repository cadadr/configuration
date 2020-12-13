#!/usr/bin/env bash
# pdfs.bash --- save or load list of open pdfs

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

fil="${PDFS_SAVE_FILE:-$HOME/pdfs.dat}"
cmd="${PDFS_OPEN_COMMAND:-okular}"

case $@ in
    dump) lsof | grep \\.pdf | awk 'FS=" /" {print "/" $2}' \
                | tail +2 | sort | uniq | tr '\n' '\0'      \
                                             > "$fil" ;
          echo Wrote $fil ;;
    load) xargs -0 $cmd < "$fil" 2>/dev/null 1>/dev/null & ;;
    *) echo "usage: $0 [dump|load]" ; exit 2 ;;
esac
