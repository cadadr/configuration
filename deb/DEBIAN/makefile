FILES=control

all: $(FILES)

include deps.mk

_DEPS!=echo $(DEPS) | sed 's: :, :g'

deps:
	@echo $(DEPS)

control: control.in deps.mk makefile
	sed 's,__MAINT__,$(MAINT),' < control.in |\
	sed 's,__VERSION__,$(VERSION),' |\
	sed "s/__DEPS__/$(_DEPS)/"> $@;

clean:
	rm -rf $(FILES)

.PHONY: all deps control clean
