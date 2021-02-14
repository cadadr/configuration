#!/usr/bin/env perl
# pomo.pl --- command line pomodoro timer

use v5.24;

use strict;
use warnings;
no warnings 'experimental::smartmatch';


# $XDG_CONFIG_HOME/pomo.d/*_hook
# scripts to be run, executable files in subprocess
# - before pomo.pl starts
# - after pomo.pl starts
# - before pomo.pl is killed
# - before pomodoro starts
# - after pomodoro starts
# - after pomodoro ends
# - before break starts
# - after break starts
# - after break ends

# command line options
# - -s | --silent : no sound
# - -n | --disable-notifications : disable desktop notifications
# - -a | --auto-advance : advance automatically, do not wait for user
#                         interaction
# - -e | --ease-in : start with an ease in period before the first
#                    pomodoro
# - -S, -L, -P, E | --short-break-secs, --long-break-secs, --pomodoro-secs,
#   --ease-in-secs SECS : durations
# - -I | --long-break-interval NUM : long break every NUM pomodoros
#
# command line options

# $XDG_CONFIG_HOME/pomo.d/config.ini
# config file
# - pomodoro, long/shot break, ease-in times
# - command line option defaults

# shell-like interface:
# $ pomo -e
# Welcome to Pomo shell!
# Type ‘help’ to get help, ‘go’ to start/resume the current phase,
# ‘pause’ to pause an ongoing phase, ‘reset’ to reset the clock
# and the counters.  See help for other commands.
#
# sound: enabled; notifications: enabled; -I: 4.
#
# Paused.
# Next phase: ease in (5min).
# > go
# Ease-in phase started.
# > pause
# Paused in ease-in phase at 4:33.
# > go
# Resuming ease-in phase, 0:27 remaining.
# >
# Ease-in phase ended (5min, completed in 6:43 with pauses).
# Next phase: pomodoro #1 (25min).
# > go
# Pomodoro #1 started.
# > status
# Ongoing phase: pomodoro #1, elapsed time 12:56, remaining: 12:04.
# > p
# Paused in pomodoro #1 at 13:11.
# > s
# Paused in phase: pomodoro #1, elapsed time 13:11, remainign: 11:49.
# > g
# Resuming pomodoro #1, 11:49 remaining.
# >
# Pomodoro #1 ended (25min, completed in 32:19 with pauses).
# Next phase: break #1 (5min).
# >
