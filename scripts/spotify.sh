#!/bin/sh
# spotify.sh --- install the spotify app.

# Spotify app is proprietary, so is not included in packages.list.
# This script is adapted from the instructions on
# <https://www.spotify.com/tr/download/linux/>.

set -e

# This might be out of date, if apt reports that the package is not
# signed, updating this from the page linked above can help.
spotify_key=931FF8E79F0876134EDDBDCCA87FF9DF48BF1C90

sudo apt-key adv --keyserver hkp://keyserver.ubuntu.com:80 \
                 --recv-keys $spotify_key

echo deb http://repository.spotify.com stable non-free \
    | sudo tee /etc/apt/sources.list.d/nonfree-spotify.list

sudo apt-get update
sudo apt-get install spotify-client
