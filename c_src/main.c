#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/IOKitLib.h>
#include <float.h>

IOHIDQueueRef queue;

struct KeyEvent {
    uint8_t type; // 0 is press, 1 is release
    uint32_t keycode;
};

void send_key(struct KeyEvent *e) {
    printf("%u %u\n", e->type, e->keycode);
}

void wait_key(struct KeyEvent *e) {
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

int grab_kb() {
    CFMutableDictionaryRef matching_dictionary = IOServiceMatching(kIOHIDDeviceKey);
    UInt32 value;
    CFNumberRef cfValue;
    value = kHIDPage_GenericDesktop;
    cfValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberSInt32Type, &value );
    CFDictionarySetValue(matching_dictionary, CFSTR(kIOHIDDeviceUsagePageKey), cfValue);
    CFRelease(cfValue);
    value = kHIDUsage_GD_Keyboard;
    cfValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberSInt32Type, &value );
    CFDictionarySetValue(matching_dictionary,CFSTR(kIOHIDDeviceUsageKey),cfValue);
    CFRelease(cfValue);
    io_service_t service = IOServiceGetMatchingService(kIOMasterPortDefault,
                                                       matching_dictionary);
    IOHIDDeviceRef device = IOHIDDeviceCreate(kCFAllocatorDefault, service);
    // grab_kb Works without actually opening the device,
    // but I'm not sure if that's guaranteed
    // IOReturn ret = IOHIDDeviceOpen(device,kIOHIDOptionsTypeNone);
    // printf("%x\n",ret);
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

int release_kb() {
    IOHIDQueueStop(queue);
    return 0;
}

int main() {
    grab_kb();
    struct KeyEvent ke;
    while(1) {
        wait_key(&ke);
        send_key(&ke);
    }
}       
