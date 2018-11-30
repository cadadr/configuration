# Makefile

# Generated configuration files:
GENCONFIGS=

ifeq ($(BASIC), yes)
	BASEPFX=basic-
else ifeq ($(BASIC), devel)
	BASEPFX=basic-
else
	BASEPFX=
endif

help:
	@echo "Targets:"
	@echo "	help			show this message"
	@echo "	install-packages	install packages from packages.list"
	@echo "	init			run system/user initialisation script"

install-packages:
ifeq ($(BASIC), yes)
	cd layers/; $(MAKE) -$(MAKEFLAGS) install-base; cd ..
else ifeq ($(BASIC), devel)
	cd layers/; $(MAKE) -$(MAKEFLAGS) install-devel; cd..
else
	cd layers/; $(MAKE) -$(MAKEFLAGS) install-full; cd ..
endif

install-config: $(GENCONFIGS)
	sudo sh ./scripts/install-configs.sh

apt-update:
	sudo apt-get update

init: install-config apt-update install-packages
	sudo sh ./scripts/$(BASEPFX)init.sh

clean:
	rm $(GENCONFIGS)

.PHONY: help init install-config pacman-update install-packages clean
