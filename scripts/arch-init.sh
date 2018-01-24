# Initialise an ArchLinux installation.

set -e

# Make sure messages are in English, as we do some cmdline parsing,
# and some utilites are kind enough to screw us with localised
# outputs.
export LC_MESSAGES=C

. /etc/os-release

configdir=/igk/config
username=g

say () {
    echo === $@
}


### Install packages:
say Installing system packages...

(
    cd $configdir;
)

### Add the user:
if id $username 2>/dev/null >/dev/null; then
    say User $username exists already.
else
    say Adding user $username...
    useradd -c 'Goktug Kayaalp' -d /home/$username \
	 -G wheel,adm -m -s /bin/bash -U -u 1993 $username \
	 || (echo Failed adding user $username && exit 1);
    groupmod -g 1993 $username || (echo Failed setting GID for $username && exit 1);
fi

### Enable daemons:
systemctl enable nginx
systemctl enable spamassassin
systemctl enable sshd
systemctl enable lightdm

say Done.  You may want to reboot your computer.
