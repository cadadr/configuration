# Setup for `sappho`

## Install system

Use the scripts found at

<https://gitlab.com/emacsomancer/full-zfs-and-full-luks-encryption-on-void-linux>

or

<https://gitlab.com/cadadr/void-zfs-luks> (my personal fork)

to set up a fully-encrypted ZFS based system.

If using Linux Mint Live ISO as installation environment, change
`Ubuntu` to `Linuxmint` in `00-pre-initialisation.sh`.

Try to use `fmt` with echo in these scripts

## Post-install

Login as `root` and run `visudo`, permit users in the `wheel` group to
run `sudo`.

Install system packages and enable services.

    # fetch setup script if necessary
    sudo xbps-install wget
    wget https://raw.githubusercontent.com/cadadr/configuration/default/systems/sappho/payload.bash https://raw.githubusercontent.com/cadadr/configuration/default/systems/sappho/checksums.txt
    sha256sum -c checksums.txt
    # run the script
    sudo ./payload.bash

Install flatpaks if desired.

    sh ../common/flatpak.sh

Install packages from my Void package repo, which is found at
<https://github.com/cadadr/void-packages>. Follow its [read me
document](https://github.com/cadadr/void-packages/blob/master/Readme.markdown)
to do so.

Reboot.

