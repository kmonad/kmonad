#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hidsystem/IOHIDShared.h>

#if !defined(__MAC_OS_X_VERSION_MIN_REQUIRED) || __MAC_OS_X_VERSION_MIN_REQUIRED >= 120000
#define KIO__PORT_DEFAULT kIOMainPortDefault
#else
#define KIO__PORT_DEFAULT kIOMasterPortDefault
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
    kern_return_t r = IOServiceGetMatchingServices(KIO__PORT_DEFAULT,
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
