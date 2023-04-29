# xdg.sh --- FreeDesktop related environment variables

# Equivalent to ~/.local/share/.
export XDG_DATA_DIRS="$MYSYSTEM/share:$MY/share${XDG_DATA_DIRS:+:$XDG_DATA_DIRS}"

for dir in "$MY/share" "$MYSYSTEM/share";
do
    # Ensure the directories exist, or login may report useless but
    # obscure errors when using a desktop session manager.
    mkdir -p "$dir"
    update-desktop-database "$dir"
done
