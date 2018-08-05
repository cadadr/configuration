# Makefile

# Generated configuration files:
GENCONFIGS=etc/netctl/home

help:
	@echo "Targets:"
	@echo "	help			show this message"
	@echo "	install-packages	install packages from packages.list"
	@echo "	init			run system/user initialisation script"

install-packages:
	perl pkglist2cmds.pl | sh -i

install-config: $(GENCONFIGS)
	@echo Installing configuration files...
	@sudo sh ./install-configs.sh

etc/netctl/home: etc/netctl/home.in
	m4 ../config.m4 $< > $@

pacman-update:
	echo TODO

init: install-config apt-update install-packages
	sh init.sh

clean:
	rm $(GENCONFIGS)

.PHONY: help init install-config pacman-update install-packages clean
