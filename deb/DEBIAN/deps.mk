# deps.mk --- Authoritative list of dependencies
### Prelude:
DEPS=
### Libraries:
DEPS+=libclass-isa-perl libswitch-perl liblockfile-bin libpam-systemd
### Utils:
DEPS+=pass telnet file reportbug lsof gawk vim tmux units		\
	debian-goodies sudo vrms xz-utils alsa-utils bzip2 less		\
	sqlite3 zip unzip python3-reportbug apt-listchanges		\
	groff-base gnupg2 lshw openssh-client bzip2 host texinfo	\
	cups-bsd cups-client bind9-host sloccount
### Services:
DEPS+=nginx syncthing openssh-server avahi-daemon avahi-autoipd
### Devtools:
DEPS+=automake autoconf autoconf-archive strace build-essential bmake	\
	ctags gdb
### Laptop:
DEPS+=laptop-mode-tools hdparm
### Desktop & Xorg:
#### Gnome:
DEPS+=gnome-core desktop-base
#### Xorg:
DEPS+=xorg xserver-xorg-input-all xserver-xorg-video-all xinput
### Fonts:
DEPS+=fonts-freefont-ttf fonts-liberation fonts-dejavu
### Apps:
DEPS+=firefox libreoffice libreoffice-help-en-gb			\
	libreoffice-help-en-us xclip gimp gimp-help-en gparted		\
	gnuplot feedgnuplot gv cheese pavucontrol mpv quodlibet		\
	eog-plugins simple-scan evince network-manager-gnome sane	\
	sane-utils gnome-maps vokoscreen qutebrowser
### Misc:
#DEPS+=gnome-session		# On Ubuntu, for normal Gnome 3 session
DEPS+=gnutls-bin cups cups-browsed cups-filters				\
	debian-archive-keyring memtest86+ pciutils netcat-traditional	\
	krb5-locales bash-completion ncurses-term mime-support		\
	pulseaudio
### Libraries:
DEPS+=libxrandr-dev
### Documentation:
DEPS+=autoconf-doc ffmpeg-doc mailutils-doc nginx-doc perl-doc		\
	python3-doc sbcl-doc gimp-help git-doc gnutls-doc doc-debian	\
	manpages man-db debian-faq gnuplot-doc bash-doc linux-doc	\
	glibc-doc glibc-doc-reference manpages-dev hyperspec doc-base
### Emacs:
DEPS+=libjansson-dev		# Jansson is for faster JSON.
DEPS+=gnutls-dev
### Internet tools:
DEPS+=bind9utils net-tools curl phantomjs corebird dnsutils w3m w3m-el	\
	webalizer wget rsync traceroute finger
# VPN
DEPS+=openvpn network-manager-openvpn-gnome resolvconf
### VCS:
DEPS+=mercurial cvs git subversion rcs python-hglib python3-hglib	\
	python-dulwich python-fastimport git-cvs gitk
### Mail:
DEPS+=clamav mailutils spamassassin spamc
### Multimedia:
DEPS+=vorbisgain youtube-dl vorbis-tools
### TeX:
DEPS+=biber texlive-xetex texlive-full texlive-publishers 		\
	texlive-bibtex-extra
### Programming languages:
#### Lisp:
DEPS+=guile-2.0 guile-2.0-doc guile-json guile-library sbcl
#### Python:
DEPS+=python-minimal python python3 python3-pip virtualenv python3-venv	\
	python-flup python3-tk python3-matplotlib
#### Perl:
DEPS+=perl perl-modules-5.24 perlbrew cpanminus liblocal-lib-perl
#### Ruby:
DEPS+=bundler ri ruby ruby-redcloth

### Rules:
list-deps:
	@echo $(DEPS)
