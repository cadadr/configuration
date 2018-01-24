# Makefile for binaries
CC=gcc
LD=gcc
CFLAGS=-std=c99 -O0
LDFLAGS=-L /usr/lib

.PHONY: all clean

all: sct of

of: optfuck.c
	$(CC) $(CFLAGS)  -o $@ $<

sct: sct.o
	$(LD) $(LDFLAGS)  $< -lm -lX11 -lXrandr -lXext  -o $@

sct.o: sct.c
	$(CC) $(CFLAGS) -o $@ -c $<

clean:
	rm -f *.o sct of
