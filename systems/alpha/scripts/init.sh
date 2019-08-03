# Initialise a Debian installation.

set -e

# Make sure messages are in English, as we do some cmdline parsing,
# and some utilites are kind enough to screw us with localised
# outputs.
export LC_MESSAGES=C

. /etc/os-release

configdir=/igk/config
username=g
userno=1993
groups=sudo,cdrom,docker,floppy,audio,dip,video,plugdev,netdev,bluetooth,lpadmin,scanner

say () {
    echo === $@
}

### Add the user:
if id $username 2>/dev/null >/dev/null; then
    say User $username exists already.
else
    say Adding user $username...
    useradd -c 'cadadr' -d /home/$username -G $groups -m -s /bin/bash -U -u $userno $username
    groupmod -g $userno $username
fi

say Done.  You may want to reboot your computer.
