export VERSION!=date +'%Y%m%d%H%M'
export MAINT="Göktuğ Kayaalp <self@gkayaalp.com>"
export DEB=goktug.deb
HERE=$(PWD)

M4=m4 config.m4

all: help

help:
	@echo "Targets:";\
	echo "	invade		run invasion";\
	echo "	build		build utilites";\
	echo "	deb		build the Debian package ($(DEB))";\
	echo "	deb-inst	install $(DEB), generating it if necessary";\
	echo "	debian-config	install system configuration for Debian";\
	echo "	debian-init	initialise new Debian system";\
	echo "	ubuntu-config	install system configuration for Ubuntu";\
	echo "	ubuntu-init	initialise new Ubuntu system";\
	echo "	rpi-config	install system configuration for Raspbian/RPi";\
	echo "	rpi-init	initialise new Raspbian/RPi system";\
	echo "version: $(VERSION)"

build: bins

bins:
	cd bin; $(MAKE) $(MAKEFLAGS); cd $(HERE)

clean-bin:
	rm -rf $(DEB); cd bin; $(MAKE) $(MAKEFLAGS) clean;\
	cd $(HERE)

invade:
	invade -v $(HOME)

all-config-files: rpi-config-files debian-config-files

# Debian/Ubuntu:
debian-init: deb-inst debian-config

DEBIANDIR=system/debian

$(DEBIANDIR)/etc/resolv.conf: $(DEBIANDIR)/etc/resolv.conf.in
	$(M4) $< > $@

$(DEBIANDIR)/etc/NetworkManager/system-connections/feriha: $(DEBIANDIR)/etc/NetworkManager/system-connections/feriha.in
	$(M4) $< > $@

DEBIANCONFFILS=$(DEBIANDIR)/etc/resolv.conf
DEBIANCONFFILS+=$(DEBIANDIR)/etc/NetworkManager/system-connections/feriha
debian-config-files: $(DEBIANCONFFILS)

export GLOBIGNORE=*.in

debian-config: debian-config-files
	cp -RPvu --preserve=mode --backup=numbered $(DEBIANDIR)/* /\
		&& locale-gen

ubuntu-init: deb-inst ubuntu-config

ubuntu-config:
	cp -RPvu --preserve=mode --backup=numbered system/ubuntu/* /

# Debian package
deb: $(DEB)

$(DEB): deb-config deb/DEBIAN/control
	dpkg-deb -b deb $@

deb-config:
	cd deb/DEBIAN; $(MAKE) $(MAKEFLAGS); cd $(HERE)

deb-touch:
	touch deb/DEBIAN/control.in

deb-check:
	lintian $(DEB)

deb-inst: deb
	apt-get install ./$(DEB) && apt-get autoremove

clean-deb:
	rm -rf *.deb; cd deb/DEBIAN; $(MAKE) $(MAKEFLAGS) clean;\
	cd $(HERE)

# Raspberry Pi:
rpi-init: rpi-setup rpi-config

RPIDIR=system/rpi

$(RPIDIR)/etc/network/interfaces: $(RPIDIR)/etc/network/interfaces.in
	$(M4) $< > $@
$(RPIDIR)/etc/wpa_supplicant/wpa_supplicant.conf: $(RPIDIR)/etc/wpa_supplicant/wpa_supplicant.conf.in
	$(M4) $< > $@

RPICONFFILS=$(RPIDIR)/etc/network/interfaces
RPICONFFILS+=$(RPIDIR)/etc/wpa_supplicant/wpa_supplicant.conf
rpi-config-files: $(RPICONFFILS)

rpi-config: rpi-config-files
	cp -RPv --preserve=mode --backup=numbered $(RPIDIR)/* /

# Clean:
clean: clean-deb clean-bin clean-configs

clean-configs:
	rm -rf $(DEBIANCONFFILS) $(RPICONFFILS)

.PHONY: all build bins deb deb-config deb-check deb-inst deb-touch clean
.PHONY: clean-bin clean-deb debian-config debian-init debian-config-files
.PHONY: rpi-init rpi-setup rpi-config rpi-config-files all-config-files
.PHONY: clean-configs

# Local variables:
# truncate-lines: nil
# End:
