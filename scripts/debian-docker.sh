#!/bin/sh
# debian-docker.sh --- Install docker on debian

set -e

key=https://download.docker.com/linux/debian/gpg
keyfile=$(mktemp)
tail=0EBFCD88

if [ $(apt-key list $tail 2>/dev/null | wc -l ) -eq 0 ]; then
    echo Download and verify Docker repo key...
    curl -fsSL $key > $keyfile
    newtail=$(gpg --with-colons $keyfile 2>/dev/null | grep ^pub |\
		  cut -d: -f5 | sed -E 's,.*(.{8})$,\1,')

    if [ ! "x$tail" = "x$newtail" ]; then
	echo "error: unexpected gpg key"
	echo "Downloaded key with signature '$newtail' does not match"
	echo "with the expected signature '$tail'.  Check $keyfile and/or"
	echo "the instuctions at"
	echo "	https://docs.docker.com/install/linux/docker-ce/debian/."
	exit 2
    fi

    echo Install the key...
    sudo apt-key add $keyfile
fi

repo=https://download.docker.com/linux/debian
lsb_release=$(lsb_release -cs)
listfile=/etc/apt/sources.list.d/docker.list

echo "deb [arch=amd64] $repo $lsb_release stable" | sudo tee $listfile

echo Install Docker CE...
sudo apt-get update
sudo apt-get install docker-ce

if grep ^docker: /etc/group > /dev/null; then
    echo Add $USER to docker group...
    sudo usermod -G docker -a $USER
fi

echo Run hello-world...
sudo docker run hello-world
