# makefile --- main makefile for .emacs.

export EMACS = $(HOME)/local/emacs/bin/emacs
export M4 = m4

help:
	@echo Targets:
	@echo '	all:		build all.'
	@echo '	clean:		clean all build artefacs.'
	@echo '	ext:		build local C extensions'
	@echo '	invade		install symlinks'
	@echo '	packages:	build emacs packages'
	@echo '	help:		print this help text.'
	@echo '	emacs:		build emacs (do NOT upgrade)'
	@echo '	emacs-up:	build emacs (upgrade)'

all: extras invade packages

packages: pdf-tools

pdf-tools:
	$(MAKE) -C lisp/pdf-tools -$(MAKEFLAGS) server/epdfinfo

ext:
	$(MAKE) -C ext -$(MAKEFLAGS)

extras:
	$(MAKE) -C extras -$(MAKEFLAGS)

invade:
	$(HOME)/bin/invade -v $(HOME)

emacs: all
	PATH="$(PWD)/bin:$(PATH)" NOFETCH=yes sh bin/build-emacs.sh

emacs-up: all
	PATH="$(PWD)/bin:$(PATH)" sh bin/build-emacs.sh

clean-ext:
	$(MAKE) -C ext -$(MAKEFLAGS) clean

clean-extras:
	$(MAKE) -C extras -$(MAKEFLAGS) clean

clean: clean-extras

.PHONY: all help ext extras docs clean clean-ext clean-docs
.PHONY: packages pdf-tools
