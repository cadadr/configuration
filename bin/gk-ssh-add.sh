#!/bin/sh
# gk-ssh-add.sh --- add my ssh identity

SSH_ASKPASS="$MYLIB/sshpass.sh" ssh-add - < ~/.ssh/id_rsa
