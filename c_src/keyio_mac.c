#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/hidsystem/IOHIDLib.h>
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

#ifdef STANDALONE
int main() {
    /*
    grab_kb();
    struct KeyEvent ke;
    while(1) {
        wait_key(&ke);
        send_key(&ke);
    }
    */
    // I think this method works (unless you run as root)
    // https://github.com/pqrs-org/Karabiner-Elements/blob/de5a74cf5d5d671672369f76495081af2715f2ba/docs/DEVELOPMENT.md
    // Note that IOHIDPostEvent is marked as deprecated
    mach_port_t iter = 0, driver = 0, service = 0, master_port = 0;
    IOReturn r = IOMasterPort(bootstrap_port, &master_port);
    if(r) return 1;
    r = IOServiceGetMatchingServices(master_port, IOServiceMatching(kIOHIDSystemClass), &iter);
    if(r) return 2;
    service = IOIteratorNext(iter);
    if(!service) return 3;
    r = IOServiceOpen(service, mach_task_self(), kIOHIDParamConnectType, &driver);
    if(r) return 4;
    //IOHIDDeviceRef device = IOHIDDeviceCreate(kCFAllocatorDefault, service);
    io_name_t devName;
    IORegistryEntryGetName(service, devName);
    printf("Device's name = %s\n", devName);
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
#endif
