# cron.sh --- common setup for cron scripts

export GK_BORG_NONINTERACTIVE=yes

env=/proc/$(pgrep -f "cinnamon-session --session cinnamon" -u "$USER" \
                | cut -d ' ' -f 1)/environ

if [ -e "$env" ]; then
    export $(cat $env | egrep -z '^DBUS_SESSION_BUS_ADDRESS=' | tr -d '\0')
fi

