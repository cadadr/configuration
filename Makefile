# makefile --- main makefile for Göktuğ's configuration.

export UMASK=

SYSTEM_CONFIG_DIR = systems/$(shell hostname)

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

invade:
	./bin/invade -v $(HOME)
	$(MAKE) -C emacs.d -$(MAKEFLAGS) invade
ifneq ($(wildcard $(SYSTEM_CONFIG_DIR)),) # if $SYSTEM_CONFIG_DIR exists, run its invasion
	cd $(SYSTEM_CONFIG_DIR) && ../../bin/invade -v $(HOME)
endif

fetch-config.m4:
	if [  -e config.m4 ]; then              \
		:;                              \
	elif [ -e ../store/config.m4 ]; then    \
		cp ../store/config.m4 .        ;\
	elif [ -e $(HOME)/Documents/config.m4 ]; then \
		cp $(HOME)/Documents/config.m4 .     ;\
	else                                    \
		touch config.m4                ;\
	fi


### Build rules:
emacs.d:
	$(MAKE) -C emacs.d -$(MAKEFLAGS) all

dotfiles:
	$(MAKE) -C dotfiles -$(MAKEFLAGS) all

src:
	$(MAKE) -C src -$(MAKEFLAGS) all


### Clean:
clean-dotfiles:
	$(MAKE) -C dotfiles -$(MAKEFLAGS) clean

clean-src:
	$(MAKE) -C src -$(MAKEFLAGS) clean

clean: clean-bin clean-dotfiles

deep-clean:
	git clean -dfx

### Postamble:
.PHONY: all build dotfiles clean src clean-src
.PHONY: clean-dotfiles deep-clean emacs.d
.PHONY: fetch-config.m4
