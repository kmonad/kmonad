.PHONY: all install

all: list-keyboards

install: list-keyboards
	mkdir -p $(DESTDIR)/bin
	cp $< $(DESTDIR)/bin

list-keyboards: list-keyboards.c
	$(CC) $< -o $@ -framework IOKit -framework CoreFoundation
