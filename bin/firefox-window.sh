#!/bin/sh

exec firefox --new-window "$(echo about:home | dmenu -p 'URL for new tab, empty for homepage')"
