# vscode.sh --- Install Visual Studio Code

# https://code.visualstudio.com/docs/setup/linux

# This script is adapted from the commands in the above link and helps
# install Visual Studio Code, which seemingly isn't available from
# Debian repos.

# TODO(2018-10-26): Stop using this if vscode becomes available from
# Debian upstream.

repo="https://packages.microsoft.com/repos/vscode"
url="https://packages.microsoft.com/keys/microsoft.asc"
fil="/tmp/microsoft-vscode-key.gpg"

curl $url | gpg --dearmor > $fil
sudo install -o root -g root -m 644 $fil /etc/apt/trusted.gpg.d/
echo "deb [arch=amd64] $repo stable main" \
  | sudo tee /etc/apt/sources.list.d/vscode.list

sudo apt-get update
sudo apt-get install code
