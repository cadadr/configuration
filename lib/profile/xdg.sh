# xdg.sh --- FreeDesktop related environment variables

# A more sensible variable to set for user configs is XDG_DATA_HOME.
# Alas, as per the spec, that variable can only hold one path, unlike
# XDG_DATA_DIRS, which can hold a list of paths. As often is the case,
# FreeDesktop succeeds in improving the already pointlessly awful
# Linux desktop experience, making it even more pointlessly awful.
# Truly, this is a major work in the discipline of collective art, and
# I have nothing but respect for it. Destruction, you see, is at the
# root of art, for thru destruction, the artifice creates an object,
# imbued with meaning. The reader must note that not all art must
# provoke joy, not all art must evoke beauty, for humanity is complex,
# for nature is manifold, and thus, the emotions expressable,
# infinite. For example, in the work of the Linux desktop developers,
# and FreeDesktop folk, we observe a perfect expression of a cesspit
# juxtaposed with a latrine that hasn’t been emptied since time
# immemorial. The putrid stench, the undoing of good, the happiness
# moribund, all are expressed clearly, but with style, almost bringing
# the word «expressionism» to my tongue. Truly masterful, the work of
# these artist-coders ultimately depict, in pristine and unmistakable
# manner, an endless hate for humanity, a vast disdain for everything
# that may inspire joy, and ultimately, constitutes a rebellion
# against the never-ending, almost eternal misconduct of those evil
# people, who, cursed be their name, want to Fucking Use Their Goddamn
# Computers In A Sensible Manner, For Fucks Sake. So Awful.

# Equivalent to ~/.local/share/.
export XDG_DATA_DIRS="$MYSYSTEM/share:$MY/share${XDG_DATA_DIRS:+:$XDG_DATA_DIRS}"

for dir in "$MY/share" "$MYSYSTEM/share";
do
    # Ensure the directories exist, or login may report useless but
    # obscure errors when using a desktop session manager.
    mkdir -p "$dir"
    update-desktop-database "$dir"
done

if which flatpak 2>/dev/null >/dev/null; then
    flatpak_data_dir="$HOME/.local/share/flatpak/exports/share"
    export XDG_DATA_DIRS="$flatpak_data_dir${XDG_DATA_DIRS:+:$XDG_DATA_DIRS}"
fi
