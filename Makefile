# makefile --- main makefile for Göktuğ's configuration.

export UMASK=
HERE:=$(PWD)
export BASIC?=no
export HOST:=$(shell hostname)

all: help

### Help:
help:
	@echo "System type: $(HOST)";\
	echo "Targets:";\
	echo "	init		initialise \`$(HOST)' instance";\
	echo "	test		test \`$(HOST)' w/ Docker (run as root)";\
	echo "	pkg		(re)install \`$(HOST)' packages";\
	echo "	conf		(re)install \`$(HOST)' config files";\
	echo "	cron		(re)install current config's cron";\
	echo "	setup		set up $$HOME and $$USER after system initialisation";\
	echo "	invade		run invasion";\
	echo "	build		build utilites and emacs.d";\
	echo "			use \`bins' and \`emacs' rules to build these";\
	echo "			separately";\
	echo "	dotfiles	build dotfiles";\
	echo "	clean		delete build artefacts";\
	echo Variables:;\
	echo "	BASIC=no/yes	Make a basic installation (default: no)"

### System initialisation:

setup: $(HOST)-setup
init: $(HOST)-init
test: $(HOST)-test
conf: $(HOST)-config
pkg: $(HOST)-pkg
cron: $(HOST)-cron

.PHONY: init test conf pkg cron setup

alpha-setup: deep-clean alpha-fetch-config.m4 build dotfiles invade cron
	sh systems/alpha/scripts/setup.sh


alpha-init: alpha-fetch-config.m4
	apt-get install -y sudo git python3 python3-distro
	$(MAKE) -C systems/alpha -$(MAKEFLAGS) init
	locale-gen


alpha-fetch-config.m4:
	if [ -e ../store/config.m4 ]; then      \
		cp ../store/config.m4 .        ;\
	elif [ -e $(HOME)/fil/config.m4 ]; then \
		cp $(HOME)/fil/config.m4 .     ;\
	else                                    \
		touch config.m4                ;\
	fi


alpha-test:
	docker build -t config-layers . && docker run config-layers

alpha-config:
	$(MAKE) -C systems/alpha -$(MAKEFLAGS) install-config

alpha-pkg:
	$(MAKE) -C systems/alpha -$(MAKEFLAGS) install-packages

alpha-cron:
	crontab cron/alpha.crontab

.PHONY: alpha-init alpha-init-sub alpha-cron alpha-setup

### Build rules:
build: bins emacs

bins:
	$(MAKE) -C bin -$(MAKEFLAGS)

emacs:
	$(MAKE) -C emacs.d -$(MAKEFLAGS) all

clean-bin:
	$(MAKE) -C bin -$(MAKEFLAGS) clean

invade: dotfiles
	./bin/invade -v $(HOME) &&\
		update-desktop-database ~/.local/share/applications/

dotfiles:
	$(MAKE) -C dotfiles -$(MAKEFLAGS)

clean-dotfiles:
	$(MAKE) -C dotfiles -$(MAKEFLAGS) clean

### Clean:
clean: clean-bin clean-dotfiles

deep-clean:
	git clean -dfx

### Postamble:
.PHONY: all build bins dotfiles clean clean-bin clean-dotfiles deep-clean

