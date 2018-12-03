# test.sh --- test alpha-debian

set -ex

apt-get update -qq && apt-get install -y -qq sudo make git equivs
make DOCKER=yes BASIC=yes alpha-debian-init
