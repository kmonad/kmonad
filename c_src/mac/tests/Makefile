clang = /usr/bin/clang
framework = -framework IOKit -framework CoreFoundation

.PHONY: test
test: ../device_properties.c
	$(clang) $^ *.c -o test.bin $(framework)
	./test.bin

.PHONY: clean
clean: test.bin
	rm test.bin
