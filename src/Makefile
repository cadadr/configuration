# src/Makefile

PROGRAMS = ../bin/i3-appropriate-layout

all: $(PROGRAMS)

../bin/i3-appropriate-layout: i3-appropriate-layout.c
	gcc -g3 -o $@ $< $(shell pkg-config --libs --cflags i3ipc-glib-1.0 xcb xcb-randr)

clean:
	rm -f $(PROGRAMS)

.PHONY: all clean
