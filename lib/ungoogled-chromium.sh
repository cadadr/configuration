#!/bin/sh
# ungoogled-chromium.sh --- set up ungoogled-chromium repo and install

. /etc/os-release

osid="Ubuntu_$(echo $UBUNTU_CODENAME | sed -E 's/^(.)(.*)/\u\1\2/')"

base="http://download.opensuse.org/repositories/home"

echo "deb $base:/ungoogled_chromium/$osid/ /" \
    | sudo tee /etc/apt/sources.list.d/home-ungoogled_chromium.list \
    > /dev/null

curl -s "$base:/ungoogled_chromium/$osid/Release.key" \
    | gpg --dearmor \
    | sudo tee /etc/apt/trusted.gpg.d/home-ungoogled_chromium.gpg \
    > /dev/null

apt update
apt install -y ungoogled-chromium ungoogled-chromium-sandbox
