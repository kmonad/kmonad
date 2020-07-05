#include <IOKit/hid/IOHIDLib.h>
#include "keyevent.h"

void hid_queue_read(struct KeyEvent *e);
int hid_queue_open(IOHIDDeviceRef device);
int hid_queue_close();
