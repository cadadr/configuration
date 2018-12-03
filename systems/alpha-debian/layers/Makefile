# Makefile

CONTROLS=$(wildcard *.control)
BASEDEBS=$(wildcard ./goktug-layer-base_*.deb)
UTILDEBS=$(wildcard ./goktug-layer-util_*.deb)
DEVELDEBS=$(wildcard ./goktug-layer-devel_*.deb)
ALLDEBS=$(wildcard ./*.deb)

INSTALL=sudo apt-get install -yqq

all:
	$(foreach c,$(CONTROLS),equivs-build $(c);)

install-full: all
	$(INSTALL) $(ALLDEBS)
	echo $(PWD)

install-base: all
	$(INSTALL) $(BASEDEBS)
	$(INSTALL) $(UTILDEBS)

install-devel: all install-base
	$(INSTALL) $(DEVELDEBS)

clean:
	rm -f *.deb

.PHONY: all install-full install-base install-devel clean
