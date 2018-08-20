# makefile --- main makefile for Göktuğ's configuration.

export VERSION!=date +'%Y%m%d%H%M'
export MAINT="Göktuğ Kayaalp <self@gkayaalp.com>"
export DEB=goktug.deb
export UMASK=
HERE=$(PWD)

all: help

### Help:
help:
	@echo "Targets:";\
	echo "	invade		run invasion";\
	echo "	build		build utilites and emacs.d";\
	echo "			use \`bins' and \`emacs' rules to build these";\
	echo "			separately";\
	echo "	dotfiles	build dotfiles";\
	echo "	clean		delete build artefacts";\

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
