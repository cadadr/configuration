# session.sh --- Background programs that accomodate a shell session.

# SSH agent:
tmpfil=$(mktemp)
ssh-agent>$tmpfil
. $tmpfil

# Bookkeeping to use notify-send from processess started outside the X
# session.  Mainly cron.
dbusfil=/tmp/${USER}-dbus
touch $dbusfil
chmod 600 $dbusfil
env | grep DBUS_SESSION_BUS_ADDRESS > $dbusfil
echo 'export DBUS_SESSION_BUS_ADDRESS' >> $dbusfil
