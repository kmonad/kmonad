#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hidsystem/IOHIDLib.h>

#include "keyevent.h"

mach_port_t driver;

int iokit_post(struct KeyEvent *e) {
    IOGPoint loc;
    loc.x = 0;
    loc.y = 0;
    NXEventData event;
    event.key.keyCode = 0x17;
    event.key.origCharCode = 0;
    event.key.repeat = 0;
    event.key.charSet = NX_SYMBOLSET;
    event.key.charCode = 0;
    event.key.origCharSet = NX_SYMBOLSET;
    event.key.keyboardType = 0;
    IOReturn kr = IOHIDPostEvent(driver, NX_KEYDOWN,
                                loc, &event, kNXEventDataVersion,
                                kIOHIDOptionsTypeNone, kIOHIDSetGlobalEventFlags);
    kr = IOHIDPostEvent(driver, NX_KEYUP,
                                loc, &event, kNXEventDataVersion,
                                kIOHIDOptionsTypeNone, kIOHIDSetGlobalEventFlags);
    return 0;
}       

int iokit_init() {
    io_service_t service = IOServiceGetMatchingService(kIOMasterPortDefault, IOServiceMatching(kIOHIDSystemClass));
    IOReturn r = IOServiceOpen(service, mach_task_self(), kIOHIDParamConnectType, &driver);
    return 0;
}

int iokit_exit() {
    kern_return_t r = IOServiceClose(driver);
    return 0;
}
