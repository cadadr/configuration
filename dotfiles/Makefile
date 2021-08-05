# dotfiles/Makefile

M4=m4 ../config.m4

FILES=mpoprc msmtprc
WGET=wget --quiet
VIMSPELLRMT=ftp://ftp.vim.org/pub/vim/runtime/spell
VIMSPELLDIR=vim/spell

all: $(FILES) vimspell

clean:
	rm -f $(FILES)

mpoprc: mpoprc.in
	$(M4) $< > $@ && chmod 600 $@

msmtprc: msmtprc.in
	$(M4) $< > $@ && chmod 600 $@

vimspell:
	mkdir -p vim/spell
	[ -e $(VIMSPELLDIR)/tr.utf-8.spl ] || $(WGET) -O \
	    $(VIMSPELLDIR)/tr.utf-8.spl $(VIMSPELLRMT)/tr.utf-8.spl
	[ -e $(VIMSPELLDIR)/en.utf-8.spl ] || $(WGET) -O \
	    $(VIMSPELLDIR)/en.utf-8.spl $(VIMSPELLRMT)/en.utf-8.spl
	[ -e $(VIMSPELLDIR)/it.utf-8.spl ] || $(WGET) -O \
	    $(VIMSPELLDIR)/it.utf-8.spl $(VIMSPELLRMT)/it.utf-8.spl

.PHONY: all clean vimspell
