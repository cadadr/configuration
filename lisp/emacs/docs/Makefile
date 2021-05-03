# makefile --- docs makefile.

OUT = onlisp.info

.PHONY: all clean

all: $(OUT)

onlisp.info: onlisp.texi
	makeinfo --fill-column=70 -o $@ $+

clean:
	$(RM) $(OUT)
