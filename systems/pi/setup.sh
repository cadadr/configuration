set -e

DEPS=hplip cups-bsd sane mercurial git phantomjs libjson-perl \
libxml-perl libxml-rss-perl postfix heirloom-mailx nginx

apt update
apt upgrade
apt install $DEPS

adduser pi lpadmin
adduser pi lp
adduser pi saned

systemctl enable saned.socket
systemctl restart saned.socket
