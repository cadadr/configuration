# Makefile

CONTROLS=$(wildcard *.control)

# Always keep the old version of configs w/o prompting.  Breaks
# automatic builds for testing otherwise.
INSTALL=sudo apt-get -o Dpkg::Options::="--force-confold" install -y

all:
	$(foreach c,$(CONTROLS),equivs-build $(c);)

install-full:
	$(INSTALL) ./goktug-layer-*.deb

install-base:
	$(INSTALL) ./goktug-layer-base_*.deb
	$(INSTALL) ./goktug-layer-util_*.deb

install-devel: install-base
	$(INSTALL) ./goktug-layer-devel_*.deb

clean:
	rm -f *.deb

.PHONY: all install-full install-base install-devel clean
