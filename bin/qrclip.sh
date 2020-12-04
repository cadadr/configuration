#!/bin/bash
xclip -selection clipboard -o | qrencode -l H -s 10 -t png -o - | display png:-
