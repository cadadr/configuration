CC = cc
X11INC = /usr/X11R6/include
X11LIB = /usr/X11R6/lib
XINERAMALIBS  = -lXinerama
XINERAMAFLAGS = -DXINERAMA
FREETYPELIBS = -lfontconfig -lXft
FREETYPEINC = /usr/include/freetype2
INCS = -I${X11INC} -I${FREETYPEINC}
LIBS = -L${X11LIB} -lX11 ${XINERAMALIBS} ${FREETYPELIBS}
VERSION = 6.1~gk0
CPPFLAGS = -D_DEFAULT_SOURCE -D_POSIX_C_SOURCE=2 -DVERSION=\"${VERSION}\" ${XINERAMAFLAGS}
CFLAGS   = -std=c99 -pedantic -Wall -Wno-deprecated-declarations -Os ${INCS} ${CPPFLAGS}
LDFLAGS  = -s ${LIBS}

OBJS=dwm.o dwm-util.o dwm-drw.o

dwm: $(OBJS) dwm-config.h
	$(CC) $(LDFLAGS) -o $@ $(OBJS)

.c.o: dwm-config.h
	$(CC) $(CFLAGS) -o $@ -c $<

clean:
	rm -f dwm $(OBJS)

.PHONY: clean
