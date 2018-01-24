# makefile --- docs makefile.

OUT = gk.info onlisp.info

.PHONY: all clean

all: $(OUT)

gk.info: gk.texi
	makeinfo --fill-column=70 -o $@ $+

onlisp.info: onlisp.texi
	makeinfo --fill-column=70 -o $@ $+

clean:
	$(RM) $(OUT)
