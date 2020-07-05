#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hidsystem/IOHIDShared.h>

IOHIDDeviceRef device_open() {
    IOHIDDeviceRef device;
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
    device = IOHIDDeviceCreate(kCFAllocatorDefault, service);
    // grab_kb Works without actually opening the device,
    // but I'm not sure if that's guaranteed
    IOReturn ret = IOHIDDeviceOpen(device,kIOHIDOptionsTypeSeizeDevice);
    if(ret == kIOReturnNotPrivileged) {
        // "As of Leopard, the kIOHIDOptionsTypeSeizeDevice option requires
        // root privileges to be used with keyboard devices."
        // https://developer.apple.com/library/archive/technotes/tn2187/_index.html
        printf("Run as root\n");
    }
    return device;
}

int device_close(IOHIDDeviceRef device) {
    IOReturn ret = IOHIDDeviceClose(device,kIOHIDOptionsTypeSeizeDevice);
    return 0;
}
