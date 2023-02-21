# Setup for `sappho`

System installation consists of three stages: base system setup: user
packages installation, and user level setup.

Use the scripts found at `stage*.bash` files, as directed by each
step.

## Stage 00: preparation

The most optimal setting to run this script is a Void Linux XFCE live
system, that is connected to internet. Alternatively, it should be
possible to use the non-graphical boot images of Void Linux, but they
may not be as convenient.

Personally, i maintain a
[Ventoy](https://www.ventoy.net/en/index.html) USB stick with many
images for this task, including both the base and XFCE images, and
also the [Hrmpf rescues
system](https://github.com/leahneukirchen/hrmpf).

**This setup is UEFI based**, and targets ThinkPad x230. It should run
fine on any system Void supports granted both stage scripts are
tweaked appropriately. The installation documentation could help with
that.

## Stage 01: base system

Run `lsblk` and make sure you know which block device will be
targeted. You’ll be prompted **once** by the script to provide that
information.

Run and attend to the [`stage01.bash`](./stage01.bash) script. It will
set up a full-disk encrypted; LUKS, LVM, and EXT4 based; seed setup that
can be booted into, with a `root` user and a user `cadadr`. Configure
variables up top in the script if necessary.

During this stage, root cadadr and LUKS passwords will be set, LUKS
password will be required multiple times.

At the end of the script, it will suggest to reboot and provide
guidance as to how to proceed after a successful reboot.

## Stage 02: prepare the system for user setup

If not already done, login as `root` and run `visudo`, permit users in
the `wheel` group to run `sudo`.

*Note* that scripts in this section are intended to be non-interactive,
and they are likely to take a long time each.

Install system packages and enable services using
[`stage02.bash`](./stage02.bash).

Install [flatpaks](../common/flatpak.sh) if desired.

    sh ../common/flatpak.sh

Install [(Xe(La))TeX packages](./tex.bash) if desired (TeX installation
is time consuming, so it’s separated and made optional. Only certain
features of my `emacs.d` depend on it).

    bash ./tex.bash

Install packages from [`new.txt`](./new.txt).

    sed 's/#.*//' new.txt | xargs sudo xbps-install -y

If the local network has a scanner, `saned` can be configured to find
it as follows (replace `<...>` with the proper hostname):

    echo <scanner server>.local | sudo tee -a /etc/sane.d/net.conf

Restart `saned` if you run the above command:

    sudo sv restart saned

Install packages from Göktuğ’s Void package repo, which is found at
<https://github.com/cadadr/void-packages>. Follow its [read me
document](https://github.com/cadadr/void-packages/blob/master/Readme.markdown)
to do so.

If you’re me and restoring your system from a backup, make sure
`~/Downloads` and `~/Efemera` directories exist.

Reboot.

## Stage 03: user set up

My login setup loads profile scripts from `~/.config/profile.d`. Some variables
relevant to this setup are expected to be set in a private script there.
A template for such a file can be found in the [`profile.d-template.sh`
file](./profile.d-template.sh).

Refer back to [the ‘Post-install’ section of the Readme file at repo
root](../../Readme.markdown#Post-install).
