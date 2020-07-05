all: sink-kext.o keyio_mac.o hid.o sink-iokit.o source-hidqueue.o
	g++ -o main $^ -framework CoreFoundation -framework IOKit

sink-kext.o: sink-kext.cpp
	g++ -c $^ -std=c++14 -IKarabiner-VirtualHIDDevice/dist/include

keyio_mac.o: keyio_mac.c
	gcc -c -DSTANDALONE $< -o $@

%.o: %.c
	gcc -c $< -o $@
