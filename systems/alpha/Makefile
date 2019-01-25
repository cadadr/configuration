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
	@echo "	install-packages	install package layers"
	@echo "	init			run system/user initialisation script"

install-packages:
ifeq ($(BASIC), yes)
	$(MAKE) -C layers/ -$(MAKEFLAGS) all install-base
else ifeq ($(BASIC), devel)
	$(MAKE) -C layers/ -$(MAKEFLAGS) all install-devel
else
	$(MAKE) -C layers/ -$(MAKEFLAGS) all install-full
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
