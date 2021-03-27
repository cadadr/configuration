#!/usr/bin/env bash
# backup-popup.bash --- run backup in xterm

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

exec xterm -T 'Backup' \
     -e 'echo Start backup process ; echo ; bash -lc do_backup ; echo ; echo Hit Return to continue... ; read'
