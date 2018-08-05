help:
	@echo "Targets:"
	@echo "	help			show this message"
	@echo "	install-packages	install packages from packages.list"
	@echo "	init			run system/user initialisation script"

install-packages:
	perl pkglist2cmds.pl | sh -i

install-config: etc/netctl/home
	cp -RPv --preserve=mode --backup=numbered etc/* /etc/

etc/netctl/home: etc/netctl/home.in
	m4 ../config.m4 $< > $@

pacman-update:
	echo TODO

init: install-config apt-update install-packages
	sh init.sh

.PHONY: help init install-config pacman-update install-packages
