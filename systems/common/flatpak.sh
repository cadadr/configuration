# flatpak.sh --- install and configure flatpaks

# Add flatpak remote
flatpak remote-add --user --if-not-exists flathub \
        https://flathub.org/repo/flathub.flatpakrepo

flatpak --noninteractive install --user fi.skyjake.Lagrange
flatpak --noninteractive install --user com.spotify.Client
flatpak --noninteractive install --user com.github.tchx84.Flatseal
flatpak --noninteractive install --user us.zoom.Zoom
flatpak --noninteractive install --user com.ozmartians.VidCutter
flatpak --noninteractive install --user com.valvesoftware.Steam
flatpak --noninteractive install --user com.vscodium.codium
flatpak --noninteractive install --user io.github.hmlendea.geforcenow-electron
flatpak --noninteractive install --user org.onlyoffice.desktopeditors

