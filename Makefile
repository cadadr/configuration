# makefile --- main makefile for Göktuğ's configuration.

export UMASK=

all: help

### Help:
help:
	@echo "System type: $(HOST)";\
	echo "Targets:";\
	echo "	setup		set up $$HOME and $$USER after system initialisation";\
	echo "	setup-light	like above, but skip building binaries";\
	echo "	emacs.d		build emacs's configs, extensions, pakcages";\
	echo "	invade		run invasion";\
	echo "	dotfiles	build dotfiles";\
	echo "	clean		delete build artefacts";\

### System initialisation:
.PHONY: setup setup-light fetch-config.m4 etc

setup: fetch-config.m4 dotfiles invade emacs.d src
	sh lib/setup.sh

setup-light: fetch-config.m4 dotfiles invade src
	@echo === Done, consider running make setup later

emacs.d:
	$(MAKE) -C emacs.d -$(MAKEFLAGS) all

fetch-config.m4:
	if [  -e config.m4 ]; then              \
		:;                              \
	elif [ -e ../store/config.m4 ]; then    \
		cp ../store/config.m4 .        ;\
	elif [ -e $(HOME)/fil/config.m4 ]; then \
		cp $(HOME)/fil/config.m4 .     ;\
	else                                    \
		touch config.m4                ;\
	fi

### Build rules:
clean-bin:
	$(MAKE) -C bin -$(MAKEFLAGS) clean

invade:
	./bin/invade -v $(HOME)
	$(MAKE) -C emacs.d -$(MAKEFLAGS) invade
	cd systems/$(shell hostname)/ && ../../bin/invade -v $(HOME)

dotfiles:
	$(MAKE) -C dotfiles -$(MAKEFLAGS)

src:
	$(MAKE) -C src -$(MAKEFLAGS)


clean-dotfiles:
	$(MAKE) -C dotfiles -$(MAKEFLAGS) clean

### Clean:
clean-src:
	$(MAKE) -C src -$(MAKEFLAGS) clean

clean: clean-bin clean-dotfiles

deep-clean:
	git clean -dfx

### Postamble:
.PHONY: all build bins dotfiles clean
.PHONY: clean-bin clean-dotfiles deep-clean emacs.d
