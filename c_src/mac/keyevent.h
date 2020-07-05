#ifndef _KEY_EVENT_
#define _KEY_EVENT_

#include <stdint.h>

#define KEY_DOWN 0
#define KEY_UP   1

struct KeyEvent {
    uint8_t type;
    uint32_t keycode;
};

#endif
