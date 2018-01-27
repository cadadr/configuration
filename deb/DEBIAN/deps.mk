# deps.mk --- Authoritative list of dependencies
### Base:
DEPS=build-essential bmake pass automake autoconf autoconf-archive	\
	texinfo nginx syncthing gnutls-bin gawk openssh-server vim	\
	tmux ctags units debian-goodies alsa-utils avahi-daemon		\
	avahi-autoipd bzip2 cups cups-browsed cups-bsd cups-client	\
	cups-filters less debian-archive-keyring gdb gnupg2 lshw man-db	\
	manpages memtest86+ openssh-client pciutils sqlite3 zip unzip	\
	sudo vrms xz-utils libclass-isa-perl python3-reportbug bzip2	\
	libswitch-perl netcat-traditional bind9-host host krb5-locales	\
	apt-listchanges groff-base reportbug lsof bash-completion	\
	liblockfile-bin ncurses-term file mime-support libpam-systemd	\
	telnet pulseaudio
### Laptop:
DEPS+=laptop-mode-tools hdparm
### Desktop & Xorg:
DEPS+=xorg xserver-xorg-input-all xserver-xorg-video-all sane sane-utils\
	dbus-x11 xinput feh dunst
### Fonts:
DEPS+=fonts-freefont-ttf fonts-liberation fonts-dejavu
### Apps:
DEPS+=firefox libreoffice libreoffice-help-en-gb			\
	libreoffice-help-en-us xclip gimp gimp-help-en gparted		\
	gnuplot gnuplot-doc feedgnuplot gv cheese pavucontrol x11-apps	\
	vtwm i3-wm i3status suckless-tools i3lock mpv quodlibet		\
	eog eog-plugins
### Misc:
#DEPS+=gnome-session		# On Ubuntu, for normal Gnome 3 session
#### Libraries:
DEPS+=libxrandr-dev
### Documentation:
DEPS+=autoconf-doc ffmpeg-doc mailutils-doc nginx-doc perl-doc		\
	python3-doc sbcl-doc gimp-help git-doc gnutls-doc doc-debian	\
	manpages man-db debian-faq
### Emacs:
DEPS+=libjansson-dev		# Jansson is for faster JSON.
DEPS+=gnutls-dev
### Internet tools:
DEPS+=bind9utils net-tools curl phantomjs corebird dnsutils w3m w3m-el	\
	webalizer wget rsync traceroute finger
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
### Lisp:
DEPS+=guile-2.0 guile-2.0-doc guile-json guile-library sbcl
### Python:
DEPS+=python-minimal python python3 python3-pip virtualenv python3-venv	\
	python-flup python3-tk python3-matplotlib
### Perl:
DEPS+=perl perl-modules-5.24 perlbrew cpanminus liblocal-lib-perl
### Ruby:
DEPS+=bundler ri ruby

list-deps:
	@echo $(DEPS)
