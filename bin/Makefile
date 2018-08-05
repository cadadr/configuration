# Makefile for binaries
HSC=ghc
HSCFLAGS=
CC=gcc
LD=gcc
CFLAGS=-std=c99 -O0
LDFLAGS=-L /usr/lib
BINS=sct of dwm

.PHONY: all clean

all: $(BINS)

xmonad: xmonad.hs
	$(HSC) $(HSCFLAGS) -o $@ $<

dwm: dwm.mk dwm*.c dwm*.h
	$(MAKE) $(MAKEFLAGS) -f dwm.mk dwm

of: optfuck.c
	$(CC) $(CFLAGS)  -o $@ $<

sct: sct.o
	$(LD) $(LDFLAGS)  $< -lm -lX11 -lXrandr -lXext  -o $@

.c.o:
	$(CC) $(CFLAGS) -o $@ -c $<

clean:
	rm -f *.o *.hi $(BINS)
	$(MAKE) $(MAKEFLAGS) -f dwm.mk clean
