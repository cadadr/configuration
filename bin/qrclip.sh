#!/bin/bash
xclip -o | qrencode -l H -s 10 -t png -o - | display png:-
