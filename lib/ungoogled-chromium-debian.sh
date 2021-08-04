#!/bin/sh
# ungoogled-chromium-debian.sh --- set up ungoogled-chromium debian repo and install

base="http://download.opensuse.org/repositories/home"

echo "deb $base:/ungoogled_chromium/Debian_Buster/ /" \
    | sudo tee /etc/apt/sources.list.d/home-ungoogled_chromium.list \
    > /dev/null

curl -s "$base:/ungoogled_chromium/Debian_Buster/Release.key" \
    | gpg --dearmor \
    | sudo tee /etc/apt/trusted.gpg.d/home-ungoogled_chromium.gpg \
    > /dev/null

apt update
apt install -y ungoogled-chromium ungoogled-chromium-sandbox
