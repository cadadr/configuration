# dotfiles/Makefile

M4=m4 ../config.m4

FILES=mpoprc msmtprc gtk-3.0/bookmarks
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

gtk-3.0/bookmarks: gtk-3.0/bookmarks.in
	$(M4) -D__USER__=$(USER) $< | sed 1d > $@

vimspell:
	mkdir -p vim/spell
	[ -e $(VIMSPELLDIR)/tr.utf-8.spl ] || $(WGET) -O \
	    $(VIMSPELLDIR)/tr.utf-8.spl $(VIMSPELLRMT)/tr.utf-8.spl
	[ -e $(VIMSPELLDIR)/en.utf-8.spl ] || $(WGET) -O \
	    $(VIMSPELLDIR)/en.utf-8.spl $(VIMSPELLRMT)/en.utf-8.spl
	[ -e $(VIMSPELLDIR)/it.utf-8.spl ] || $(WGET) -O \
	    $(VIMSPELLDIR)/it.utf-8.spl $(VIMSPELLRMT)/it.utf-8.spl

.PHONY: all clean vimspell
