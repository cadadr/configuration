# install-packages.sh --- install packages from list

grep '^ - ' packages.list	\
	| sed 's,^ - ,,'	\
	| sudo xargs apt-get install -y
