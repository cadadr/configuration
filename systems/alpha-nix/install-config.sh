#!/bin/sh
# install-config.sh --- install the config expr and rebuild & switch

cp -Rup configuration.nix /etc/nixos/ && nixos-rebuild switch
