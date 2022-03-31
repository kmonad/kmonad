#include <unistd.h>

/*
 * Key event information that's shared between C++ and Haskell.
 *
 * type: represents key up or key down
 * page: represents IOKit usage page
 * usage: represents IOKit usage
 */
struct KeyEvent {
    uint64_t type;
    uint32_t page;
    uint32_t usage;
};

// defined in keyio_mac.cpp
void print_iokit_error(const char *fname, int freturn = 0);
