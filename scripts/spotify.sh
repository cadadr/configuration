#!/bin/sh
# spotify.sh --- install the spotify app.

# Spotify app is proprietary, so is not included in goktug.deb.  This
# script is adapted from the instructions on
# <https://www.spotify.com/tr/download/linux/>.

set -e

sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
                 --recv-keys 0DF731E45CE24F27EEEB1450EFDC8610341D9410

echo deb http://repository.spotify.com stable non-free \
    | sudo tee /etc/apt/sources.list.d/nonfree-spotify.list

sudo apt-get update
sudo apt-get install spotify-client
