# test.sh --- test alpha-debian

set -ex

apt-get update -qq && apt-get install -y -qq sudo make git python3 python3-distro
make DOCKER=yes BASIC=yes alpha-init
