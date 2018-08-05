# Makefile for binaries
HSC=ghc
HSCFLAGS=
CC=gcc
LD=gcc
CFLAGS=-std=c99 -O0
LDFLAGS=-L /usr/lib
BINS=sct of xmonad

.PHONY: all clean

all: $(BINS)

xmonad: xmonad.hs
	$(HSC) $(HSCFLAGS) -o $@ $<

of: optfuck.c
	$(CC) $(CFLAGS)  -o $@ $<

sct: sct.o
	$(LD) $(LDFLAGS)  $< -lm -lX11 -lXrandr -lXext  -o $@

.c.o:
	$(CC) $(CFLAGS) -o $@ -c $<

clean:
	rm -f *.o *.hi $(BINS)
