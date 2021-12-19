# xdg.sh --- FreeDesktop related environment variables

# Equivalent to ~/.local/share/.
export XDG_DATA_DIRS="$MYSYSTEM/share:$MY/share:$XDG_DATA_DIRS"

for dir in "$MY/share" "$MYSYSTEM/share";
do
    # Ensure the directories exist, or login may report useless but
    # obscure errors when using a desktop session manager.
    mkdir -p "$dir"
    update-desktop-database "$dir"
done

# Extend ~/.config.
export XDG_CONFIG_DIRS="$MY/xdg:/etc/xdg"
# Normally we’d want to include $XDG_CONFIG_DIRS above to include
# default directories but for some reason it doesn’t work at least on
# Cinnamon so I’m manually adding /etc/xdg. We *don’t* want ~/.config
# in here, it’s for XDG_CONFIG_HOME.

