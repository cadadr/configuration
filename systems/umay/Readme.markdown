# Setup recipe for `umay`

After first boot, set a root password for when `sudo` breaks (and it
does, trust me)

    $ sudo su
    # passwd

and then enable source repositories using the "Software Sources" GUI
tool.  Preferably also install any updates Linux Mint may ask you to
install.

The following commands can be run from inside vi(1)/vim(1) using `y$`
for yanking a command, and `:! Ctrl+R " <CR>` to paste it to the
command line and execute it.

If you need to set up an encrypted drive, see [this
document](./CryptSetup.markdown).

Now it's time to get going with the installation process.

The `install.*` fileset contains a listing of Debian packages to be
installed, and the file `build-dep` contains a listing of
packages for which to fetch build dependencies.  The could be
installed by the following commands:

    $ cat install.* | grep -v '^#' | sudo xargs apt-get install -y
    $ sudo xargs apt-get build-dep -y < build-dep

It might be the case that errors are reported for some packages, most
probably because of a clash with the config files we installed into
`etc`.  Normally, `dpkg` prompts for these clashes, but because output
will not be a TTY here, those prompts will be skipped.  If you find
yourself in this situation, run

    $ sudo apt-get install -f

After this, you might want to install documentation for the installed
packages.  There is a script for that:

    $ sudo sh ../../lib/install-doc-packages.sh

This will inspect the dpkg database and find out all the relevant
`-doc` packages, and install them.

If you’ll use msmtp, the following command makes AppArmor quit its
stupid bullshit.  I’ve tried aliases to no avail.

    $ sudo aa-complain usr.bin.msmtp

Disable PulseAudio power management (insert `\\` before `#` in the
command below to escape it when calling from vi/vim command line):

    $ sudo sed -i -E 's/^(load-module module-suspend-on-idle)$/# \1/' /etc/pulse/default.pa

Finally, update locales as necessary:

    $ printf 'tr_TR.UTF-8 UTF-8\nen_GB.UTF-8 UTF-8\nen_US.UTF-8 UTF-8\n' | sudo tee /etc/locale.gen
    $ sudo locale-gen

Now it's time to go back to repo root and follow the instructions in
the [Readme there](../../Readme.markdown). **Before this, reboot, and
log in on a virtual console, using `Ctrl+Alt+F1--6`**.

---

![screen cap](/candy/scr-umay.png)
