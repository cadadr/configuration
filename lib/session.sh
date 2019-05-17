# session.sh --- Background programs that accomodate a shell session.

# SSH agent:
tmpfil=$(mktemp)
ssh-agent>$tmpfil
. $tmpfil
