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

Allow non-root users to use ping because why not?

    sysctl -w net.ipv4.ping_group_range="0 1000"

Then, switch to the regular user and, likely using `nmcli`, to connect
to a network.

Claim ownership of home directory.

    cd
    sudo chown $USER:$USER .

Install system packages and enable services

    sudo ./packages.bash

Log out and log back in so that `elogind` is activated.
