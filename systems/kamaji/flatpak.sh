# flatpak.sh --- install and configure flatpaks

flatpak --noninteractive install fi.skyjake.Lagrange
flatpak --noninteractive install com.spotify.Client
flatpak --noninteractive install org.telegram
flatpak --noninteractive install io.bit3.WhatsAppQT
flatpak --noninteractive install com.github.tchx84.Flatseal

flatpak override --user --filesystem=$MYFS/dat/lagrange fi.skyjake.Lagrange
