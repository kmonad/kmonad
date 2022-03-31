#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hidsystem/IOHIDShared.h>
#include <stdlib.h> // free

#include "device_properties.h"

int main() {
    CFMutableDictionaryRef matching_dictionary = IOServiceMatching(kIOHIDDeviceKey);
    if(!matching_dictionary) {
        fprintf(stderr,"IOServiceMatching failed");
        return 1;
    }

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

    io_iterator_t iter = IO_OBJECT_NULL;
    kern_return_t r = IOServiceGetMatchingServices(kIOMasterPortDefault,
                                                   matching_dictionary,
                                                   &iter);
    if(r != KERN_SUCCESS) {
        fprintf(stderr,"IOServiceGetMatchingServices failed");
        return r;
    }

    struct device_properties *properties = malloc(sizeof *properties);
    if (properties == NULL)
    {
        fprintf(stderr, "malloc error: device properties");
        return 1;
    }

    printf("Note: only prints non-empty fields.\n\n");
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
        get_device_properties(curr, properties);
        print_device_properties(properties);
    }

    free(properties);
    return 0;
}
