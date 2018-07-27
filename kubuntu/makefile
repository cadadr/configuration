help:
	@echo "Targets:"
	@echo "	help			show this message"
	@echo "	install-packages	install packages from packages.list"
	@echo "	init			run system/user initialisation script"

install-packages:
	perl pkglist2cmds.pl | sh

install-config:
	cp -RPv --preserve=mode --backup=numbered etc/* /etc/

apt-update:
	apt-get update

init: install-config apt-update install-packages
	sh init.sh

.PHONY: help init install-config apt-update install-packages
