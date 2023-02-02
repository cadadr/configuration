# cron.sh --- common setup for cron scripts

# Figure out DBus info.
if [ -e /var/lib/dbus/machine-id ] \
    && [ -e "$HOME/.dbus/session-bus/$(cat /var/lib/dbus/machine-id)-0" ];
then
    . $HOME/.dbus/session-bus/$(cat /var/lib/dbus/machine-id)-0
    export DBUS_SESSION_BUS_ADDRESS DBUS_SESSION_BUS_PID
    export DBUS_SESSION_BUS_WINDOWID
else
    if pgrep i3 >/dev/null 2>&1; then
        env=/proc/$(pgrep -f "i3 " -u "$USER" | cut -d ' ' -f 1)/environ
    elif pgrep cinnamon-session >/dev/null 2>&1; then
        env=/proc/$(pgrep -f "cinnamon-session --session cinnamon" -u "$USER" \
                          | cut -d ' ' -f 1)/environ
    else
        echo $0: could not locate session process
    fi

    if [ -e "$env" ]; then
        export $(cat $env | egrep -z '^DBUS_SESSION_BUS_ADDRESS=' | tr -d '\0')
    fi
fi
