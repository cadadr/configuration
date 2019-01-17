# makefile --- main makefile for Göktuğ's configuration.

export UMASK=
HERE:=$(PWD)
export BASIC?=no

all: help

### Help:
help:
	@echo "Targets:";\
	echo "	alpha-init		initialise alpha instance with Debian";\
	echo "	alpha-test		test alpha w/ Docker";\
	echo "	alpha-config		install alpha config files";\
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

alpha-init:
	touch config.m4; $(MAKE) -C systems/alpha -$(MAKEFLAGS) init

alpha-test:
	docker build -t config-layers . && docker run config-layers

alpha-config:
	$(MAKE) -C systems/alpha -$(MAKEFLAGS) install-config

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
