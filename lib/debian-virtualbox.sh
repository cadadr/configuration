#!/bin/sh
# debian-virtualbox.sh --- install virtualbox from upstream

. /etc/os-release

echo "deb [arch=amd64] https://download.virtualbox.org/virtualbox/debian $DEBIAN_CODENAME contrib" > /etc/apt/sources.list.d/gk-virtualbox.list

wget -q https://www.virtualbox.org/download/oracle_vbox_2016.asc -O- | sudo apt-key add -
wget -q https://www.virtualbox.org/download/oracle_vbox.asc -O- | sudo apt-key add -

apt-get update
apt-get install virtualbox-6.1
