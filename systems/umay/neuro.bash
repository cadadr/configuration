#!/usr/bin/env bash
# neuro.bash --- install neurodebian

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

apt-get install neurodebian
apt-get update
apt-get install psychopy octave-psychtoolbox-3 python-pyepl
