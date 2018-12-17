# makefile --- main makefile for Göktuğ's configuration.

export UMASK=
HERE:=$(PWD)
export BASIC?=no

all: help

### Help:
help:
	@echo "Targets:";\
	echo "	alpha-debian-init	initialise alpha instance with Debian";\
	echo "	alpha-debian-test	test alpha-debian w/ Docker";\
	echo "	alpha-debian-config	install alpha-debian config files";\
	echo "	pi-init			initialise pi instance";\
	echo "	invade			run invasion";\
	echo "	build			build utilites and emacs.d";\
	echo "				use \`bins' and \`emacs' rules to build these";\
	echo "				separately";\
	echo "	dotfiles		build dotfiles";\
	echo "	clean			delete build artefacts";\
	echo Variables:;\
	echo "	BASIC=no/yes		Make a basic installation (default: no)"

### System initialisation:

alpha-debian-init:
	touch config.m4; $(MAKE) -C systems/alpha-debian -$(MAKEFLAGS) init

alpha-debian-test:
	docker build -t config-layers . && docker run config-layers

alpha-debian-config:
	$(MAKE) -C systems/alpha-debian -$(MAKEFLAGS) install-config

### Build rules:
build: bins emacs

bins:
	$(MAKE) -C bin -$(MAKEFLAGS)

emacs:
	$(MAKE) -C emacs.d -$(MAKEFLAGS) all

clean-bin:
	$(MAKE) -C bin -$(MAKEFLAGS) clean

invade: dotfiles
	./bin/invade -v $(HOME)

dotfiles:
	$(MAKE) -C dotfiles -$(MAKEFLAGS)

clean-dotfiles:
	$(MAKE) -C dotfiles -$(MAKEFLAGS) clean

### Clean:
clean: clean-bin

### Postamble:
.PHONY: all build bins dotfiles clean clean-bin clean-dotfiles
.PHONY: alpha-init alpha-init-sub
