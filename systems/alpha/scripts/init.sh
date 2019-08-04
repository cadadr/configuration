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

### Update the user:
say Updating user $username...
usermod -a -G $groups -s /bin/bash -U -u $userno $username
groupmod -g $userno $username

say Done.  You may want to reboot your computer.
