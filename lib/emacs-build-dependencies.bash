# emacs-build-dependencies.bash --- install emacs build and runtime dependencies

if [ "$UID" -ne 0 ]; then
    echo $(basename $0): error: this script requires superuser privileges
    exit 2
fi

case "$(lsb_release -si)" in
    Debian|Ubuntu|Linuxmint)
	apt-get update
	apt-get build-dep emacs emacs-pdf-tools ;;
    VoidLinux)
	xbps-install -S # update repos
	xbps-install -y $(xbps-query -R --fulldeptree -x emacs-gtk3)
	xbps-install -y jansson-devel ncurses-devel libXaw-devel  \
		gtk+-devel gtk+3-devel webkit2gtk-devel dbus-devel \
		acl-devel libjpeg-turbo-devel tiff-devel giflib-devel \
		libpng-devel libXpm-devel librsvg-devel libmagick-devel \
		libxml2-devel alsa-lib-devel m17n-lib-devel harfbuzz-devel \
		cairo-devel gmp-devel gnutls-devel
	;;
esac

