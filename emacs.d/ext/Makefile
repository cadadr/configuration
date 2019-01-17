# makefile --- Makefile for local emacs modules.

EMACS_ROOT = $(HOME)/co/External/emacs

CC = gcc
LD = gcc
CFLAGS = -ggdb3 -Wall -I$(EMACS_ROOT)/src -fPIC
LDFLAGS = -shared

ALLMODS=example

all: $(ALLMODS)

example: gk-example.so

gk-example.so: gk-module.o gk-example.o
	$(LD) $(LDFLAGS) -o $@ $+

gk-module.o: gk-module.c
	$(CC) $(CFLAGS) -o $@ -c $+

gk-example.o: gk-example.c
	$(CC) $(CFLAGS) -o $@ -c $+

clean:
	rm -f *.o *.so

.MAIN: all

.PHONY: clean all $(ALLMODS)
