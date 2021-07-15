# qtile.py --- qtile window manager config

# Note: original licence block moved to the end of file.

import math
import os
import socket

from collections import namedtuple
from typing import List  # noqa: F401

from libqtile import bar, layout, widget
from libqtile.config import Click, Drag, Group, Key, Match, Screen
from libqtile.lazy import lazy
from libqtile.utils import guess_terminal


### Options:

mod = "mod4"
terminal = "kitty"
thesis_graph_size = 25
hostname = socket.gethostname()

dgroups_key_binder = None
dgroups_app_rules = []  # type: List
main = None  # WARNING: this is deprecated and will be removed soon
follow_mouse_focus = True
bring_front_click = False
cursor_warp = False
auto_fullscreen = True
focus_on_window_activation = "smart"

# (Original comment from default Qtile config.py.)
#
# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"


### Keyboard bindings:

keys = []

def k(*bindings, **props):
    """
    Conveniently bind keys.

    Positional arguments are keys.

    The `do' keyword argument binds the key to whatever callable passed
    in, the `run' keyword instead wraps it in a call to `lazy.spawn()'.
    """
    if 'do' in props:
        action = props['do']
    elif 'run' in props:
        action = lazy.spawn(props['run'])
    else:
        raise KeyError("function k() must have a do= or a run=")

    keys.extend(
            [
                Key(bindings[:-1], bindings[-1], action, desc=props['desc'])
            ]
    )


# Switch between windows
k(mod, "h", do=lazy.layout.left(), desc="Move focus to left")
k(mod, "l", do=lazy.layout.right(), desc="Move focus to right")
k(mod, "j", do=lazy.layout.down(), desc="Move focus down")
k(mod, "k", do=lazy.layout.up(), desc="Move focus up")
k(mod, "space", do=lazy.layout.next(), desc="Move window focus to other window")

# Move windows between left/right columns or move up/down in current stack.
# Moving out of range in Columns layout will create new column.
k(mod, "shift", "h", do=lazy.layout.shuffle_left(), desc="Move window to the left")
k(mod, "shift", "l", do=lazy.layout.shuffle_right(), desc="Move window to the right")
k(mod, "shift", "j", do=lazy.layout.shuffle_down(), desc="Move window down")
k(mod, "shift", "k", do=lazy.layout.shuffle_up(), desc="Move window up")

# Grow windows. If current window is on the edge of screen and direction
# will be to screen edge - window would shrink.
k(mod, "control", "h", do=lazy.layout.grow_left(), desc="Grow window to the left")
k(mod, "control", "l", do=lazy.layout.grow_right(), desc="Grow window to the right")
k(mod, "control", "j", do=lazy.layout.grow_down(), desc="Grow window down")
k(mod, "control", "k", do=lazy.layout.grow_up(), desc="Grow window up")
k(mod, "n", do=lazy.layout.normalize(), desc="Reset all window sizes")

# Toggle between split and unsplit sides of stack.
# Split = all windows displayed
# Unsplit = 1 window displayed, like Max layout, but still with
# multiple stack panes
k(mod, "shift", "Return", do=lazy.layout.toggle_split(),
    desc="Toggle between split and unsplit sides of stack")

# Toggle between different layouts as defined below
k(mod, "Tab", do=lazy.next_layout(), desc="Toggle between layouts")
k(mod, "End", do=lazy.window.kill(), desc="Kill focused window")

k("F12", run="rofi -show drun", desc="Spawn a command using a prompt widget")

k(mod, "g", do=lazy.restart(), desc="Restart Qtile")
k(mod, "control", "shift", "q", do=lazy.shutdown(), desc="Shutdown Qtile")

k(mod, "Return", run=terminal, desc="Launch terminal")
k(mod, "control", "Return", run="emacsclient -c", desc="Launch emacsclient")

k(mod, "control", "f", run="pcmanfm", desc="Launch file manager")
k(mod, "control", "w", run="ffprofile.sh",
    desc="Launch firefox profile selector")
k(mod, "control", "p", run="passmenu.sh", desc="Launch password picker")
k(mod, "control", "b", run="backup-popup.bash",
        desc="Launch borg backup popup")
k(mod, "Next", run="pkill -USR1 -f setbg.bash", desc="Next wallpaper")
k(mod, "Prior", run="pkill -USR2 -f setbg.bash", desc="Previous wallpaper")
k(mod, "q", run="qrclip.sh", desc="Clipboard to QR code")


### Mouse bindings:

# Drag floating layouts.
mouse = [
    Drag([mod], "Button1", lazy.window.set_position_floating(),
         start=lazy.window.get_position()),
    Drag([mod], "Button3", lazy.window.set_size_floating(),
         start=lazy.window.get_size()),
    Click([mod], "Button2", lazy.window.bring_to_front())
]



### Workspaces:

groups = [
        Group("1:home"),
        Group("2:read"),
        Group("3:rsc"),
        Group("4:anon"),
        Group("5:anon"),
    ]

for i, g in zip(range(1, 5), groups):
    key = f"F{i}"
    k(key, do=lazy.group[g.name].toscreen(),
            desc="Switch to group {}".format(g.name))

    k("shift", key, do=lazy.window.togroup(g.name),
        desc="move focused window to group {}".format(g.name))


### Bars:

widget_defaults = dict(
    font='DejaVu Sans Mono',
    fontsize=12,
    padding=3,
)
extension_defaults = widget_defaults.copy()
screens = []


def spacer_widget():
    return widget.TextBox(text=" | ", foreground="#00ff7f")


def thesis_progress():
    try:
        fp = os.path.expanduser("~/Notes/MastersThesis/Notes.org")
        with open(fp, "r") as fh:
            for line in fh:
                if line.startswith('* reading list'):
                    progress_line = line
        progress = progress_line.split(" ")[3]
        percent = math.trunc(int(progress[1:-2]) / (100 / thesis_graph_size))
        done = "".join(['█' for _ in range(0, percent)])
        left = "".join(['·' for _ in range(0, thesis_graph_size - percent)])
        graph = f'|{done}{left}|'
        return widget.TextBox(text=f"thesis readings: {graph}",
                foreground="#ffff00")
    except Exception as e:
        print(e)
        return widget.TextBox(text='thesis reading: ??')


top_bar_widgets = [
    widget.CurrentLayout(),
    widget.GroupBox(),
    widget.Prompt(),
    widget.Chord(
        chords_colors={
            'launch': ("#ff0000", "#ffffff"),
        },
        name_transform=lambda name: name.upper(),
    ),
    widget.WindowName(),
    widget.TextBox(text=hostname),
    spacer_widget(),
    thesis_progress(),
    spacer_widget(),
    widget.Clock(format='%Y-%m-%d %a %I:%M %p'),
    spacer_widget(),
    widget.Systray(),
]

top_bar = bar.Bar(top_bar_widgets, 24) 

scr1 = Screen(top=top_bar)

screens.append(scr1)


### Layouts:
layouts = [
    layout.Columns(
        border_width = 3,
        margin = 10
    ),
    layout.Max(),
]

floating_layout = layout.Floating(float_rules=[
    Match(title='pinentry'),  # GPG key password entry
])


### Original licence block:

# Copyright (c) 2010 Aldo Cortesi
# Copyright (c) 2010, 2014 dequis
# Copyright (c) 2012 Randall Ma
# Copyright (c) 2012-2014 Tycho Andersen
# Copyright (c) 2012 Craig Barnes
# Copyright (c) 2013 horsik
# Copyright (c) 2013 Tao Sauvage
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.

