#include <IOKit/hid/IOHIDLib.h>
#include <float.h>

#include "keyevent.h"

IOHIDQueueRef queue;

void hid_queue_read(struct KeyEvent *e) {
    while(1) {
        // The following call doesn't block, even though the docs say it should:
        // https://developer.apple.com/documentation/iokit/1545832-iohidqueuecopynextvaluewithtimeo
        IOHIDValueRef value = IOHIDQueueCopyNextValueWithTimeout(queue,DBL_MAX);
        if(value) {
            CFIndex integer_value = IOHIDValueGetIntegerValue(value);
            IOHIDElementRef element = IOHIDValueGetElement(value);
            uint32_t usage_page = IOHIDElementGetUsagePage(element);
            uint32_t usage = IOHIDElementGetUsage(element);
            if((usage_page == kHIDPage_KeyboardOrKeypad &&
                kHIDUsage_KeyboardErrorUndefined < usage &&
                usage < kHIDUsage_Keyboard_Reserved) ||
               (usage_page == 0xff)) { // See AppleHIDUsageTables.h
                e->type = !integer_value;
                e->keycode = usage;
                return;
            }
        }
    }
}

int hid_queue_open(IOHIDDeviceRef device) {
    queue = IOHIDQueueCreate(kCFAllocatorDefault, device, 32, kIOHIDOptionsTypeNone);
    CFArrayRef elements = IOHIDDeviceCopyMatchingElements(device,
                                                          NULL,
                                                          kIOHIDOptionsTypeNone);
    for (CFIndex i = 0; i < CFArrayGetCount(elements); ++i) {
        IOHIDQueueAddElement(queue,
                             (IOHIDElementRef)CFArrayGetValueAtIndex(elements, i));
    }
    CFRelease(elements);
    IOHIDQueueStart(queue);
    return 0;
}

int hid_queue_close() {
    IOHIDQueueStop(queue);
    return 0;
}
