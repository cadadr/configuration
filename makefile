# makefile --- main makefile for Göktuğ's configuration.

export UMASK=
HERE:=$(PWD)
export BASIC?=no

all: help

### Help:
help:
	@echo "Targets:";\
	echo "	alpha-init	initialise alpha instance";\
	echo "	pi-init		initialise pi instance";\
	echo "	invade		run invasion";\
	echo "	build		build utilites and emacs.d";\
	echo "			use \`bins' and \`emacs' rules to build these";\
	echo "			separately";\
	echo "	dotfiles	build dotfiles";\
	echo "	clean		delete build artefacts";\
	echo Variables:;\
	echo "	BASIC=no/yes	Make a basic installation (default: no)"

### System initialisation:

alpha-init:
	touch config.m4; cd systems/alpha;\
		 $(MAKE) -$(MAKEFLAGS) init; cd $(HERE)

### Build rules:
build: bins emacs

bins:
	cd bin; $(MAKE) -$(MAKEFLAGS); cd $(HERE)

emacs:
	cd emacs.d; $(MAKE) -$(MAKEFLAGS) all; cd $(HERE)

clean-bin:
	rm -rf $(DEB); cd bin; $(MAKE) -$(MAKEFLAGS) clean;\
	cd $(HERE)

invade: dotfiles
	./bin/invade -v $(HOME)

dotfiles:
	cd dotfiles; $(MAKE) -$(MAKEFLAGS); cd $(HERE)

clean-dotfiles:
	cd dotfiles; $(MAKE) -$(MAKEFLAGS) clean; cd $(HERE)

### Clean:
clean: clean-bin

### Postamble:
.PHONY: all build bins dotfiles clean clean-bin clean-dotfiles
.PHONY: alpha-init alpha-init-sub
