# makefile --- main makefile for .emacs.

export EMACS = /home/g/local/bin/emacs
export M4 = m4

help:
	@echo Targets:
	@echo '	all:	build all.'
	@echo '	clean:	clean all build artefacs.'
	@echo '	docs:	build local documentation.'
	@echo '	ext:	build local C extensions'
	@echo '	help:	print this help text.'

all: extras docs

ext:
	cd ext; $(MAKE) $(MAKEFLAGS)

extras:
	cd extras; $(MAKE) $(MAKEFLAGS)

docs:
	cd docs; $(MAKE) $(MAKEFLAGS)

clean-ext:
	cd ext; $(MAKE) $(MAKEFLAGS) clean

clean-extras:
	cd extras; $(MAKE) $(MAKEFLAGS) clean

clean-docs:
	cd docs; $(MAKE) $(MAKEFLAGS) clean

clean: clean-extras clean-docs

.PHONY: all help ext extras docs clean clean-ext clean-docs
