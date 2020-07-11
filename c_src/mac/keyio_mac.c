#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hidsystem/IOHIDShared.h>
#include <unistd.h>
#include <pthread.h>
#include "sink-kext.h"
#include "keyevent.h"

static mach_port_t sink;
static pthread_t thread_id;
static IOHIDDeviceRef *source_device;
static int n_devices;
static int fd[2];

void callback(void *context, IOReturn result, void *sender, IOHIDValueRef value) {
    struct KeyEvent e;
    CFIndex integer_value = IOHIDValueGetIntegerValue(value);
    IOHIDElementRef element = IOHIDValueGetElement(value);
    uint16_t usage_page = IOHIDElementGetUsagePage(element);
    uint16_t usage = IOHIDElementGetUsage(element);
    uint32_t keycode = (usage_page << 16) | usage;
    e.type = !integer_value;
    e.keycode = keycode;
    write(fd[1], &e, sizeof(struct KeyEvent));
}

int send_key(struct KeyEvent *e) {
    kext_post(e);
    return 0;
}

int wait_key(struct KeyEvent *e) {
    read(fd[0], e, sizeof(struct KeyEvent));
    return 0;
}

void *monitor_kb(void *ctx) {
    for(int i = 0; i < n_devices; i++) {
        IOHIDDeviceScheduleWithRunLoop(source_device[i], CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
    }
    CFRunLoopRun();
    return NULL;
}

io_iterator_t get_device_iterator() {
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
    io_iterator_t iter = 0;
    kern_return_t r = IOServiceGetMatchingServices(kIOMasterPortDefault,
                                                   matching_dictionary,
                                                   &iter);
    return iter;
}

void list_kb() {
    io_iterator_t iter = get_device_iterator();
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
        CFTypeRef str = IORegistryEntryCreateCFProperty(curr,
                                                        CFSTR("Product"),
                                                        kCFAllocatorDefault,
                                                        kIOHIDOptionsTypeNone);
        CFShow(str);
    }
}

int grab_kb(char *product) {
    // Source
    io_iterator_t iter = get_device_iterator();
    mach_port_t *service = NULL;
    n_devices = 0;
    // would be nice to replace this with C++ to use a lambda...
    if(product) {
        // This should be achieved in the matching dictionary, but wouldn't work
        CFStringRef cf_product = CFStringCreateWithCString(kCFAllocatorDefault, product, CFStringGetSystemEncoding());
        for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
            CFTypeRef cf_curr_product = IORegistryEntryCreateCFProperty(curr, CFSTR(kIOHIDProductKey), kCFAllocatorDefault, kIOHIDOptionsTypeNone);
            if(CFStringCompare(cf_product, cf_curr_product, 0) == kCFCompareEqualTo) {
                n_devices++;
            }
        }
        service = (mach_port_t *)malloc(sizeof(mach_port_t) * n_devices);
        IOIteratorReset(iter);
        int i = 0;
        for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
            CFTypeRef cf_curr_product = IORegistryEntryCreateCFProperty(curr, CFSTR(kIOHIDProductKey), kCFAllocatorDefault, kIOHIDOptionsTypeNone);
            if(CFStringCompare(cf_product, cf_curr_product, 0) == kCFCompareEqualTo) {
                service[i] = curr;
                i++;
            }
        }
    } else {
        for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
            n_devices++;
        }
        service = (mach_port_t *)malloc(sizeof(mach_port_t) * n_devices);
        IOIteratorReset(iter);
        int i = 0;
        for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
            service[i] = curr;
            i++;
        }
    }
    source_device = (IOHIDDeviceRef *)malloc((sizeof (IOHIDDeviceRef)) * n_devices);
    for(int i = 0; i < n_devices; i++) {
        source_device[i] = IOHIDDeviceCreate(kCFAllocatorDefault, service[i]);
        IOHIDDeviceRegisterInputValueCallback(source_device[i], callback, NULL);
        IOReturn ret = IOHIDDeviceOpen(source_device[i],kIOHIDOptionsTypeSeizeDevice);
        if(ret == kIOReturnNotPrivileged) {
            printf("Run as root\n");
        }
    }
    if (pipe(fd) == -1) { 
        fprintf(stderr, "Pipe Failed" ); 
        return 1; 
    }
    pthread_create(&thread_id, NULL, monitor_kb, NULL);
    // Sink
    kext_init();
    return 0;
}

int release_kb() {
    // Source
    for(int i = 0; i < n_devices; i++) {
        IOReturn ret = IOHIDDeviceClose(source_device[i],kIOHIDOptionsTypeSeizeDevice);
    }
    pthread_cancel(thread_id);
    // Sink
    kext_exit();
    return 0;
}

#ifdef STANDALONE
int main() {
    list_kb();
}       
#endif
