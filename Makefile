# makefile --- main makefile for Göktuğ's configuration.

export UMASK=

all: help

### Help:
help:
	@echo "System type: $(HOST)";\
	echo "Targets:";\
	echo "	setup		set up $$HOME and $$USER after system initialisation";\
	echo "	setup-light	like above, but skip building binaries";\
	echo "	install-doc-packages";\
	echo "			install all relevant documentation packages";\
	echo "	invade		run invasion";\
	echo "	build		build utilites and emacs.d";\
	echo "	dotfiles	build dotfiles";\
	echo "	clean		delete build artefacts";\

### System initialisation:
.PHONY: setup setup-light fetch-config.m4 install-doc-packages

setup: fetch-config.m4 dotfiles invade
	sh lib/setup.sh

setup-light: fetch-config.m4 dotfiles invade
	@echo === Done, consider running make setup later

fetch-config.m4:
	if [ -e ../store/config.m4 ]; then      \
		cp ../store/config.m4 .        ;\
	elif [ -e $(HOME)/fil/config.m4 ]; then \
		cp $(HOME)/fil/config.m4 .     ;\
	else                                    \
		touch config.m4                ;\
	fi

install-doc-packages:
	sudo sh lib/install-doc-packages.sh

### Build rules:
build: emacs

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
