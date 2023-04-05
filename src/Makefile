# src/Makefile

PROGRAMS = ../bin/i3-appropriate-layout

### Parameters:
DEBUG = no
PEDANTIC = no
CC = gcc
C_FLAGS_EXTRA =

### C flags:
C_FLAGS = -Wformat=2 -Wfloat-equal -Wundef -Wshadow -Wpointer-arith -Wcast-align
C_FLAGS += -Wdouble-promotion -Wstrict-prototypes -Wwrite-strings
C_PEDANTIC = -Wcast-qual  -Wstrict-overflow=5
C_DEBUG_FLAGS += -g3 -Og -Wextra -Wall -pedantic-errors  -Waggregate-return
C_DEBUG_FLAGS += -Wswitch-default -Wswitch-enum -Wconversion -Wunreachable-code
C_DEBUG_FLAGS += -fverbose-asm -finstrument-functions --coverage

ifeq ($(CC),clang)
# Can’t seem to make libubsan work with GCC for now.
C_DEBUG_FLAGS += -fsanitize=undefined
else ifeq ($(CC),gcc)
C_DEBUG_FLAGS += -fsanitize-address-use-after-scope
endif

ifeq ($(DEBUG),yes)
C_FLAGS += $(C_DEBUG_FLAGS)
else
C_FLAGS += -O3 -Wstrict-overflow=4
endif

ifeq ($(PEDANTIC),yes)
C_FLAGS += $(C_PEDANTIC)
endif

C_FLAGS += $(C_FLAGS_EXTRA)

### Recipes:
#### Meta:
help:
	@echo "Variables:";\
	echo "	CC		C compiler (+)		(default=$(CC))";\
	echo "	DEBUG		Turn on debug builds(*)	(default=$(DEBUG))";\
	echo "	PEDANTIC	Build pedantically(*)	(default=$(PEDANTIC))";\
	echo "	C_FLAGS_EXTRA	Extra C build flags	(default=$(C_FLAGS_EXTRA))";\
	echo "		(This option configures builds to warn about and";\
	echo "		 sanitize every remotely useful thing possible,";\
	echo "		 meant to be used along with DEBUG=yes)";\
	echo "	(*) These options accept yes/no as answers";\
	echo "	(+) \`clang' and \`gcc' supported, $(CC) is the default";\
	echo "Recipes:";\
	echo "	all		build everything";\
	echo "	help		show this help message (default recipe)";\
	echo "	clean		delete generated files";\
	echo "Executable build targets:";\
	echo "	$(PROGRAMS)" | fmt;\
	echo "Full C flags selected:";\
	echo "	$(C_FLAGS)" | fmt

all: $(PROGRAMS)

#### i3-appropriate-layout:
../bin/i3-appropriate-layout: i3-appropriate-layout.c
	$(CC) $(C_FLAGS) $(shell pkg-config --libs --cflags i3ipc-glib-1.0 xcb xcb-randr) -o $@ $<

### Cleaning:
clean:
	rm -f $(PROGRAMS)

### Phonies:
.PHONY: help all clean
