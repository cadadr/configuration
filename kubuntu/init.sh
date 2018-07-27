# Initialise a Kubuntu installation.

set -e

# Make sure messages are in English, as we do some cmdline parsing,
# and some utilites are kind enough to screw us with localised
# outputs.
export LC_MESSAGES=C

. /etc/os-release

configdir=/igk/config
username=g
userno=1993

say () {
    echo === $@
}

### Add the user:
if id $username 2>/dev/null >/dev/null; then
    say User $username exists already.
else
    say Adding user $username...
    useradd -c 'Goktug Kayaalp' -d /home/$username \
	 -G sudo,adm,cdrom,dip,plugdev,lpadmin,sambashare \
	 -m -s /bin/bash -U -u $userno $username \
	|| echo Failed adding user $username # && exit 1;
    groupmod -g $userno $username \
	|| echo Failed setting GID for $username
fi

### Enable services:
say Enabling system services...
for service in spamassassin ssh nginx; do
	systemctl enable $service || true
done

say Enabling user services...
for service in syncthing; do
	systemctl --user enable $service || true
done

say Done.  You may want to reboot your computer.
