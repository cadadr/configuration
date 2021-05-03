# makefile --- extras makefile.

OUT = batch-mail.el mailto.el

.PNOHY: all clean

all: $(OUT)

clean:
	$(RM) $(OUT)

%.el: el.m4 %.el.in
	$(M4) -D__EMACS__=$(EMACS) $+ >$@ && chmod 750 $@
