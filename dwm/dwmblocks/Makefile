DESTDIR := /usr/local/bin
CC      := cc
CFLAGS  := -pedantic -Wall -Wno-deprecated-declarations -Os
LDFLAGS := -lX11

all: options dwmblocks

options:
	@echo dwmblocks build options:
	@echo "DESTDIR = ${DESTDIR}"
	@echo "CFLAGS  = ${CFLAGS}"
	@echo "LDFLAGS = ${LDFLAGS}"
	@echo "CC      = ${CC}"

dwmblocks: dwmblocks.c blocks.def.h blocks.h
	${CC} -o dwmblocks dwmblocks.c ${CFLAGS} ${LDFLAGS}

blocks.h:
	cp blocks.def.h $@

clean:
	rm -f *.o *.gch dwmblocks

install: dwmblocks
	cp -f dwmblocks ${DESTDIR}
	chmod 755 ${DESTDIR}/dwmblocks

uninstall:
	rm -f ${DESTDIR}/dwmblocks

.PHONY: all options clean install uninstall
