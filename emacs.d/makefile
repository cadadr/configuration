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
	cd ext; $(MAKE) -$(MAKEFLAGS); cd ..

extras:
	cd extras; $(MAKE) -$(MAKEFLAGS); cd ..

docs:
	cd docs; $(MAKE) -$(MAKEFLAGS); cd ..

clean-ext:
	cd ext; $(MAKE) -$(MAKEFLAGS) clean; cd ..

clean-extras:
	cd extras; $(MAKE) -$(MAKEFLAGS) clean; cd ..

clean-docs:
	cd docs; $(MAKE) -$(MAKEFLAGS) clean; cd ..

clean: clean-extras clean-docs

.PHONY: all help ext extras docs clean clean-ext clean-docs
