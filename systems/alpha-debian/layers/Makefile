# Makefile

CONTROLS=$(wildcard *.control)
BASEDEBS=$(wildcard ./goktug-layer-base_*.deb)
UTILDEBS=$(wildcard ./goktug-layer-util_*.deb)
DEVELDEBS=$(wildcard ./goktug-layer-devel_*.deb)
ALLDEBS=$(wildcard ./goktug-layer-*.deb)

INSTALL=sudo apt-get install -yqq

all:
	$(foreach c,$(CONTROLS),equivs-build $(c);)

install-full: all
	if [ -z "$(strip $(ALLDEBS))" ]; then echo ALLDEBS EMPTY\!; exit 3; fi
	$(INSTALL) $(ALLDEBS)

install-base: all
	if [ -z "$(strip $(BASEDEBS))" ]; then echo ALLDEBS EMPTY\!; exit 3; fi
	if [ -z "$(strip $(UTILDEBS))" ]; then echo ALLDEBS EMPTY\!; exit 3; fi
	$(INSTALL) $(BASEDEBS)
	$(INSTALL) $(UTILDEBS)

install-devel: all install-base
	if [ -z "$(strip $(DEVELDEBS))" ]; then echo ALLDEBS EMPTY\!; exit 3; fi
	$(INSTALL) $(DEVELDEBS)

clean:
	rm -f *.deb

.PHONY: all install-full install-base install-devel clean
