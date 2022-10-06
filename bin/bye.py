#!/usr/bin/env python3
# bye.py --- suspend/shutdown-via-dbus helper using rofi

import subprocess

dbus_cmd_prefix = [
    "dbus-send", "--system", "--print-reply", "--dest=org.freedesktop.login1",
    "/org/freedesktop/login1",
]

commands = {
    "suspend":   dbus_cmd_prefix + ["org.freedesktop.login1.Manager.Suspend", "boolean:true"],
    "sleep":     dbus_cmd_prefix + ["org.freedesktop.login1.Manager.Suspend", "boolean:true"],
    "hibernate": dbus_cmd_prefix + ["org.freedesktop.login1.Manager.Hibernate", "boolean:true"],
    "reboot":    dbus_cmd_prefix + ["org.freedesktop.login1.Manager.Reboot", "boolean:true"],
    "shutdown":  dbus_cmd_prefix + ["org.freedesktop.login1.Manager.PowerOff", "boolean:true"],
    "power off": dbus_cmd_prefix + ["org.freedesktop.login1.Manager.PowerOff", "boolean:true"],
}

try:
    choice = subprocess.run(
        ["dmenu"],
        check=True,
        input=bytes("\n".join(commands.keys()), encoding="utf-8"),
        capture_output=True
    ).stdout.decode(encoding="utf-8").strip()
except subprocess.CalledProcessError as e:
    print(f"rofi exited with nonzero exit status ({e.returncode})")
    exit(e.returncode)

try:
    subprocess.run(commands[choice], check=True)
except subprocess.CalledProcessError as e:
    print(f"dbus-send exited with nonzero exit status ({e.returncode})")
    exit(e.returncode)

