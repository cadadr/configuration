# guix.sh --- Guix(SD) environment

type source >/dev/null || source () {
        . $@
}

GUIX_PROFILE="$HOME/.guix-profile"

if [ -f "$GUIX_PROFILE" ]; then
    export GUIX_PROFILE

    # Import generated environment variables
    source $GUIX_PROFILE/etc/profile
fi

