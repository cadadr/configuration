# dotfiles/Makefile

M4=m4 ../config.m4

FILES=mpoprc msmtprc

all: $(FILES) vimspell

clean:
	rm -f $(FILES)

mpoprc: mpoprc.in
	$(M4) $< > $@ && chmod 600 $@

msmtprc: msmtprc.in
	$(M4) $< > $@ && chmod 600 $@

vimspell:
	mkdir -p vim/spell
	wget --quiet -O vim/spell/tr.utf-8.spl ftp://ftp.vim.org/pub/vim/runtime/spell/tr.utf-8.spl
	wget --quiet -O vim/spell/en.utf-8.spl ftp://ftp.vim.org/pub/vim/runtime/spell/en.utf-8.spl
	wget --quiet -O vim/spell/it.utf-8.spl ftp://ftp.vim.org/pub/vim/runtime/spell/it.utf-8.spl

.PHONY: all clean vimspell
