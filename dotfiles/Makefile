# dotfiles/Makefile

M4=m4 ../config.m4

FILES=mpoprc msmtprc

all: $(FILES)

clean:
	rm -f $(FILES)

mpoprc: mpoprc.in
	$(M4) $< > $@ && chmod 600 $@

msmtprc: msmtprc.in
	$(M4) $< > $@ && chmod 600 $@

.PHONY: all clean
