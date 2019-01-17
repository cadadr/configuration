M4=m4 ../config.m4

help:
	@echo "Targets:";\
	@echo "	init	initialise RPi"
	@echo "	config	install configuration"

init: config
	sh setup.sh

etc/network/interfaces: etc/network/interfaces.in
	$(M4) $< > $@; chmod 644 $@
etc/wpa_supplicant/wpa_supplicant.conf: etc/wpa_supplicant/wpa_supplicant.conf.in
	$(M4) $< > $@; chmod 644 $@

config: etc/network/interfaces etc/wpa_supplicant/wpa_supplicant.conf
	cp -RPv --preserve=mode --backup=numbered etc/* /etc/
