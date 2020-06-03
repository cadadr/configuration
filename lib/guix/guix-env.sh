# guix-env.sh --- GNU Guix setup

export GUIX_PROFILE="$HOME/.guix-profile"

if [ -d "$GUIX_PROFILE"]; then
    . "$GUIX_PROFILE/etc/profile"

    export MY_GUIX_LIB="$MYLIB/guix/"

    export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
    export GUIX_LOCPATH="$GUIX_PROFILE/lib/locale"
fi

