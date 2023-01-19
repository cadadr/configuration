#!/usr/bin/env bash
# gk-ssh-add.bash --- interactively add an ssh key to ssh-agent(1)

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

# This script is for helping facilitate the addition of SSH keys to
# ssh-agent(1), thru interactive selection.
#
# The user is prompted for a key and to pick a matching password file
# from pass(1)’s database. A script that calls pass(1) to print that
# password to stdout is created as a shim which is then used as
# SSH_ASKPASS.
#
# pass(1) prompts for GPG password, decrypts and prints out the key to
# the stdout of the shim script, which is then picked up by ssh-add(1)
# itself.
#
# This is initally created for use with Magit to facilitate credential
# use while remoting.

if [ -n "$GK_SSH_ADD_DOMAIN" ]; then
    # Try to locate identity file.
    maybe_idfile="$(sed -nE "/^Host $GK_SSH_ADD_DOMAIN/,/^Host/ p" \
                        < ~/.ssh/config      \
                        | grep IdentityFile  \
                        | awk '{print($2)}')" && {
        key="$(basename --suffix=.pub $maybe_idfile)"
    }
fi

if [[ -v key ]]; then
    : # We’re set to go, otherwise ask user to pick a key.
else
    key="$(basename --multiple --suffix=.pub ~/.ssh/*.pub \
                      | dmenu -p 'Pick ssh key to add to ssh-agent(1)' -i)"
fi

# If the key is already cached in the agent, exit
ssh-add -l | cut -d ' ' -f2 \
    | grep "$(ssh-keygen -l -f ~/.ssh/aur | cut -d ' ' -f2)" >/dev/null \
    && \
    {
        notify-send "~/.ssh/$key is already cached in ssh-agent"
        exit
    }

password="$(pass git ls-files | grep ^ssh | sed s/.gpg\$// \
                 | dmenu -p "Pick password for \`$key'" -i)"

askpass_shim=$(mktemp -t askpass_shimXXXXXXXXXXXX.sh)

chmod 700 $askpass_shim

cat > $askpass_shim <<EOF
#!/bin/sh
pass show $password
EOF

SSH_ASKPASS_REQUIRE=force SSH_ASKPASS="$askpass_shim" ssh-add ~/.ssh/$key

notify-send "ssh-agent has now cached ~/.ssh/$key"

exec rm $askpass_shim
