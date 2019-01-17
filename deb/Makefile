# makefile

all: help

help:
	@echo "Targets:"
	@echo "	debs		build pakcages"
	@echo "	install		install packages"

debs: oddmuse-deb

install: oddmuse-install

oddmuse-install: oddmuse-deb
	sudo apt-get install ./oddmuse*.deb

oddmuse-deb:
	equivs-build oddmuse.control

.PHONY: all debs oddmuse-install oddmuse-deb

