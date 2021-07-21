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


### Gruvbox colours:
black='#282828'
black0='#7c6f64'
red='#cc241d'
red0='#fb4934'
green='#98971a'
green0='#b8bb26'
yellow='#d79921'
yellow0='#fabd2f'
blue='#458588'
blue0='#83a598'
purple='#b16286'
purple0='#d3869b'
aqua='#689d6a'
aqua0='#8ec07c'
white='#a89984'
white0='#fbf1c7'


### Options:

mod = "mod4"
terminal = "kitty"
thesis_graph_size = 20
hostname = socket.gethostname()
username = os.environ.get("USER", "??")

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

    Optionally, `desc' keyword argument can be used to add a description
    to the keybinding.
    """
    if 'do' in props:
        action = props['do']
    elif 'run' in props:
        action = lazy.spawn(props['run'])
    else:
        raise KeyError("function k() must have a do= or a run=")

    keys.extend(
            [
                Key(bindings[:-1], bindings[-1], action,
                    desc=props.get('desc'))
            ]
    )


# Switch between windows
k(mod, "h", do=lazy.layout.left())
k(mod, "l", do=lazy.layout.right())
k(mod, "j", do=lazy.layout.down())
k(mod, "k", do=lazy.layout.up())
k(mod, "space", do=lazy.layout.next())

# Move windows between left/right columns or move up/down in current stack.
# Moving out of range in Columns layout will create new column.
k(mod, "shift", "h", do=lazy.layout.shuffle_left())
k(mod, "shift", "l", do=lazy.layout.shuffle_right())
k(mod, "shift", "j", do=lazy.layout.shuffle_down())
k(mod, "shift", "k", do=lazy.layout.shuffle_up())

# Grow windows. If current window is on the edge of screen and direction
# will be to screen edge - window would shrink.
k(mod, "control", "h", do=lazy.layout.grow_left())
k(mod, "control", "l", do=lazy.layout.grow_right())
k(mod, "control", "j", do=lazy.layout.grow_down())
k(mod, "control", "k", do=lazy.layout.grow_up())
k(mod, "n", do=lazy.layout.normalize())

# Toggle between split and unsplit sides of stack.
# Split = all windows displayed
# Unsplit = 1 window displayed, like Max layout, but still with
# multiple stack panes
k(mod, "shift", "Return", do=lazy.layout.toggle_split())

# Toggle between different layouts as defined below
k(mod, "Tab", do=lazy.next_layout())
k(mod, "End", do=lazy.window.kill())

k("F12", run="rofi -show drun")

k(mod, "g", do=lazy.restart())
k(mod, "control", "shift", "q", do=lazy.shutdown())
# Kill picom, it sometimes causes a freeze.
k(mod, "control", "shift", "p",
    run="bash -c 'killall -9 picom ; picom -c'")

k(mod, "Return", run=terminal)
k(mod, "control", "Return", run="emacsclient -c")

k(mod, "control", "f", run="pcmanfm")
k(mod, "control", "w", run="ffprofile.sh")
k(mod, "control", "p", run="passmenu.sh")
k(mod, "control", "b", run="backup-popup.bash")
k(mod, "Next", run="pkill -USR1 -f setbg.bash")
k(mod, "Prior", run="pkill -USR2 -f setbg.bash")
k(mod, "q", run="qrclip.sh")


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

for i, g in zip(range(1, 6), groups):
    key = f"F{i}"
    k(key, do=lazy.group[g.name].toscreen())

    k("shift", key, do=lazy.window.togroup(g.name))


### Bars:

widget_defaults = dict(
    font='DejaVu Sans Mono',
    fontsize=14,
    padding=3,
)
extension_defaults = widget_defaults.copy()
screens = []


def thesis_progress(**kwargs):
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
        return widget.TextBox(text=f"thesis readings: {graph} {progress}", **kwargs)
    except Exception as e:
        print(e)
        return widget.TextBox(text='thesis reading: ??',
                background=red0, foreground=white0)


top_bar_widgets = [
    widget.CurrentLayoutIcon(
        background=black,
        foreground=white,
        padding=10
    ),
    widget.GroupBox(
        background=blue,
        foreground=black,
        padding=10,
        active=black,
        inactive=black,
        highlight_method="line",
        highlight_color=[blue0, blue0]
    ),
    widget.Prompt(),
    widget.Chord(
        chords_colors={
            'launch': (red, white),
        },
        name_transform=lambda name: name.upper(),
        background=black
    ),
    widget.Spacer(background=black),
    widget.TextBox(text=" ", background=black),
    widget.Clock(
        format='%Y-%m-%d %a <b>%I:%M</b> %p',
        background=white0,
        foreground=black,
        padding=10
    ),
    widget.TextBox(text=" ", background=black),
    thesis_progress(
        background=yellow0,
        foreground=black,
        padding=10
    ),
    widget.TextBox(text=" ", background=black),
    widget.Systray(
        background=black,
        padding=10
    ),
    widget.TextBox(text=" ", background=black),
]

top_bar = bar.Bar(top_bar_widgets, 24)

bottom_bar_widgets= [
    widget.TextBox(text=" ", background=black),
    widget.TextBox(
        text=f"{username}@{hostname}",
        foreground=black,
        background=green0,
        padding=10
    ),
    widget.TextBox(text=" ", background=black),
    widget.WindowName(
        background=aqua,
        foreground=black,
        padding=10
    ),
    widget.TextBox(text=" ", background=black),
    widget.Net(
        format="↓{down}{up} ↑",
        background=green,
        foreground=white0,
        padding=10
    ),
    widget.TextBox(text=" ", background=black),
    widget.Memory(
        background=blue0,
        foreground=black,
        padding=10
    ),
    widget.TextBox(text=" ", background=black),
    widget.CPU(
        background=purple0,
        foreground=black,
        padding=10
    ),
    widget.TextBox(text=" ", background=black),
    widget.ThermalSensor(
        background=red,
        foreground=white0,
        padding=10
    ),
    widget.TextBox(text=" ", background=black),
]

bottom_bar = bar.Bar(bottom_bar_widgets, 24)

scr1 = Screen(top=top_bar, bottom=bottom_bar)

screens.append(scr1)

common_layout_config = {
    "border_width": 3,
    "border_focus": yellow0,
    "border_normal": black0,
    "margin": 15
}

### Layouts:
layouts = [
    layout.Columns(
        **common_layout_config
    ),
    layout.Max(),
    layout.MonadTall(
        **common_layout_config
    ),
    layout.MonadWide(
        **common_layout_config
    ),
]

floating_layout = layout.Floating(float_rules=[
    # Run the utility of `xprop` to see the wm class and name of an X client.
    *layout.Floating.default_float_rules,
    Match(wm_class='ssh-askpass'),
    Match(title='pinentry'),
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

