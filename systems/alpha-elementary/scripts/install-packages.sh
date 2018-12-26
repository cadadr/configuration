# install-packages.sh --- install packages from list

set -e

sudo apt-get update

grep '^ - ' packages.list	\
	| sed 's,^ - ,,'	\
	| sudo xargs apt-get install -y
