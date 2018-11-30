# Makefile

CONTROLS!=ls *.control

all:
	$(foreach c,$(CONTROLS),equivs-build $(c);)

install-full: all
	sudo apt-get install ./*.deb

install-base: all
	sudo apt-get install ./goktug-base-*.deb
	sudo apt-get install ./goktug-util-*.deb

install-devel: all install-base
	sudo apt-get install ./goktug-devel-*.deb

clean:
	rm -f *.deb

.PHONY: all install-full install-base install-devel clean
