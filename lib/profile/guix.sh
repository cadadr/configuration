# guix.sh --- Guix(SD) environment

GUIX_PROFILE="$HOME/.guix-profile"

if [ -f "$GUIX_PROFILE" ]; then
    export GUIX_PROFILE

    # Import generated environment variables
    . $GUIX_PROFILE/etc/profile
fi

