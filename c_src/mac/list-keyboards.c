#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hidsystem/IOHIDShared.h>

/* The name was changed from "Master" to "Main" in Apple SDK 12.0 (Monterey) */
/* MAC_OS_X_VERSION_12_0 does not exist :( */
#if MAC_OS_X_VERSION_MAX_ALLOWED < 120000
    #define kIOMainPortDefault kIOMasterPortDefault
#endif

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
    kern_return_t r = IOServiceGetMatchingServices(kIOMainPortDefault,
                                                   matching_dictionary,
                                                   &iter);
    if(r != KERN_SUCCESS) {
        fprintf(stderr,"IOServiceGetMatchingServices failed");
        return r;
    }
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
        CFTypeRef str = IORegistryEntryCreateCFProperty(curr,
                                                        CFSTR("Product"),
                                                        kCFAllocatorDefault,
                                                        kIOHIDOptionsTypeNone);
        CFShow(str);
    }
    return 0;
}
