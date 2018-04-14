# Initialise a Debian/Ubuntu installation.

set -e

# Make sure messages are in English, as we do some cmdline parsing, and some utilites are kind enough to screw us with localised outputs.
export LC_MESSAGES=C

. /etc/os-release

configdir=/igk/config
username=g
userno=1993

say () {
    echo === $@
}

### Install required tools:
apt install -y make curl apt-transport-https

### Add third-party repos:
#### Syncthing:
curl -s https://syncthing.net/release-key.txt | apt-key add -
echo 'deb https://apt.syncthing.net/ syncthing stable' >\
    /etc/apt/sources.list.d/syncthing.list
chmod 644 /etc/apt/sources.list.d/syncthing.list

### Install packages:
say Installing system packages...
apt update

(
    cd $configdir;
    if which make >/dev/null 2>/dev/null; then
	make debian-init
    else
	apt install -y ./goktug.deb && make debian-config
    fi \
	|| (echo Failed installing system programmes && exit 1)
)

### Add the user:
if id $username 2>/dev/null >/dev/null; then
    say User $username exists already.
else
    say Adding user $username...
    useradd -c 'Goktug Kayaalp' -d /home/$username \
	 -G sudo,adm,cdrom,dip,plugdev -m -s /bin/bash -U -u $userno $username \
	|| echo Failed adding user $username && exit 1;
    groupmod -g $userno $username || echo Failed setting GID for $username && exit 1;
fi

### Start system services:
say Starting system services...
for service in spamassassin ssh nginx; do
    (systemctl status $service | grep 'enabled;') >/dev/null \
	|| systemctl enable $service;
    (systemctl status $service | grep 'active (running)') >/dev/null \
	|| systemctl start $service;
done

say Done.  You may want to reboot your computer.
