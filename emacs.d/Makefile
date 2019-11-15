# makefile --- main makefile for .emacs.

export EMACS = /home/g/local/emacs/bin/emacs
export M4 = m4

help:
	@echo Targets:
	@echo '	all:		build all.'
	@echo '	clean:		clean all build artefacs.'
	@echo '	ext:		build local C extensions'
	@echo '	packages:	build emacs packages
	@echo '	help:		print this help text.'

all: extras packages

ext:
	$(MAKE) -C ext -$(MAKEFLAGS)

extras:
	$(MAKE) -C extras -$(MAKEFLAGS)

packages:
	$(MAKE) -C lisp/site -$(MAKEFLAGS)

clean-ext:
	$(MAKE) -C ext -$(MAKEFLAGS) clean

clean-extras:
	$(MAKE) -C extras -$(MAKEFLAGS) clean

clean: clean-extras

.PHONY: all help ext extras docs clean clean-ext clean-docs
