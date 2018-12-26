# Makefile

# Generated configuration files:
GENCONFIGS=

help:
	@echo "Targets:"
	@echo "	help			show this message"
	@echo "	install-packages	install packages from packages.list"
	@echo "	install-config		install configuration files"
	@echo "	init			run system/user initialisation script"

install-packages:
	sh ./scripts/install-packages.sh

install-config: $(GENCONFIGS)
	sudo sh ./scripts/install-configs.sh

apt-update:
	sudo apt-get update

init: install-config apt-update install-packages
	@echo Initialise system...

clean:
	rm $(GENCONFIGS)

.PHONY: help init install-config pacman-update install-packages clean
