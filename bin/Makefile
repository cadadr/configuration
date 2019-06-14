# Makefile for binaries
CC=gcc
GHC=ghc
LD=gcc
CFLAGS=-std=c99 -O0
LDFLAGS=-L /usr/lib
BINS=sct of bright

.PHONY: all clean dwm-clean

all: $(BINS)

of: optfuck.c
	$(CC) $(CFLAGS)  -o $@ $<

sct: sct.o
	$(LD) $(LDFLAGS)  $< -lm -lX11 -lXrandr -lXext  -o $@

.c.o:
	$(CC) $(CFLAGS) -o $@ -c $<

bright: bright.hs
	$(GHC) -o $@ $<

clean:
	rm -f *.o *.hi $(BINS)
