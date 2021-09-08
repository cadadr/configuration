# Makefile

# Build all packages that need any compilation.

all:
	cd pdf-tools/server && ./autogen.sh all && ./configure
	$(MAKE) -C pdf-tools/server -$(MAKEFLAGS)
