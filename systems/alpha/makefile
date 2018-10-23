# Makefile

# Generated configuration files:
GENCONFIGS=

help:
	@echo "Targets:"
	@echo "	help			show this message"
	@echo "	install-packages	install packages from packages.list"
	@echo "	init			run system/user initialisation script"

install-packages:
	perl pkglist2cmds.pl | sh -i

install-config: $(GENCONFIGS)
	sudo sh ./install-configs.sh

apt-update:
	sudo apt-get update

init: install-config apt-update install-packages
	sudo sh init.sh

clean:
	rm $(GENCONFIGS)

.PHONY: help init install-config pacman-update install-packages clean
