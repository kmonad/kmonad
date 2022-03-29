clang = /usr/bin/clang
framework = -framework IOKit -framework CoreFoundation

all: list-keyboards.c device_properties.c
	$(clang) $^ -o list-keyboards $(framework)

