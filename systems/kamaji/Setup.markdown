# Setup recipe for `kamaji`

Install using the Linux Mint Cinnamon ISO, but should work for the LMDE version
as well, without requiring much change.

The commands listed here are to be installed after the first boot, inside the
graphical session, before running the invasion inside `/igk`.

After first boot, set a root password for when `sudo` breaks (and it
does, trust me)

    $ sudo su
    # passwd

and then enable source repositories using the Software sources app.

The following commands can be run from inside vi(1)/vim(1) using `y$`
for yanking a command, and `:! Ctrl+R " <CR>` to paste it to the
command line and execute it.  The following binding can help make that
easier:

    :nmap ,! 02wy$:!<C-r>"

If you need to set up an encrypted drive, see [this
document](./CryptSetup.markdown).

Now it's time to get going with the installation process.

The `install.*` fileset contains a listing of Debian packages to be
installed, and the file `build-dep` contains a listing of
packages for which to fetch build dependencies.  The could be
installed by the following commands:

    $ sudo apt-get install $(cat install.* | grep -v '^\#.*')
    $ sudo apt-get build-dep $(cat build-dep)

It might be the case that errors are reported for some packages, most
probably because of a clash with the config files we installed into
`etc`.  Normally, `dpkg` prompts for these clashes, but because output
will not be a TTY here, those prompts will be skipped.  If you find
yourself in this situation, run

    $ sudo apt-get install -f

Now, install Ungoogled Chromium:

    $ sudo sh ../../lib/ungoogled-chromium.sh

After this, you might want to install documentation for the installed
packages.  There is a script for that:

    $ sudo sh ../../lib/install-doc-packages.sh

This will inspect the dpkg database and find out all the relevant
`-doc` packages, and install them.

We can now also install flatpaks:

    $ sh ./flatpak.sh

If you’ll use msmtp, the following command makes AppArmor quit its
stupid bullshit.  I’ve tried aliases to no avail.

    $ sudo aa-complain usr.bin.msmtp

My printer/scanner is exposed to LAN through [a Raspberry
Pi](../ayata).  The following configuration modification allows
`saned` to access it:

    $ echo ayata.local | sudo tee -a /etc/sane.d/net.conf

Finally, install the `gTile` Cinnamon spice using the `Extensions`
app.

Now it's time to go back to repo root and follow the instructions in
the [Readme there](../../Readme.markdown). **Before this, reboot, and
log in on a virtual console, using `Ctrl+Alt+F1--6`**.

---

![screen cap](/candy/scr-kamaji.png)
