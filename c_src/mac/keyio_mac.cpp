#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hidsystem/IOHIDShared.h>
#include <stdlib.h> // malloc, free
#include <unistd.h>
#include <errno.h>
#include <thread>
#include <map>
#include <iostream>
#include <mach/mach_error.h>

#include "common.hpp"
extern "C" {
  #include "device_properties.h"
}

// From kext.cpp or dext.cpp
int init_sink(void);
int exit_sink(void);

/*
 * These are needed to receive unaltered key events from the OS.
 */
static std::thread thread;
static CFRunLoopRef listener_loop;
static std::map<io_service_t,IOHIDDeviceRef> source_device;
static int fd[2];

void print_iokit_error(const char *fname, int freturn /* = 0 */) {
    std::cerr << fname << " error";
    if(freturn) {
        //std::cerr << " " << std::hex << freturn;
        std::cerr << ": ";
        std::cerr << mach_error_string(freturn);
    }
    std::cerr << std::endl;
}

/* Make a CFString out of a C string */
static CFStringRef make_cfstring(const char *str)
{
    return CFStringCreateWithCString(kCFAllocatorDefault,
                                     str,
                                     kCFStringEncodingUTF8);
}

/*
 * We'll register this callback to run whenever an IOHIDDevice
 * (representing a keyboard) sends input from the user.
 *
 * It passes the relevant information into a pipe that will be read
 * from with wait_key.
 */
static void input_callback(void *context, IOReturn result, void *sender, IOHIDValueRef value) {
    struct KeyEvent e;
    IOHIDElementRef element = IOHIDValueGetElement(value);
    e.type = IOHIDValueGetIntegerValue(value);
    e.page = IOHIDElementGetUsagePage(element);
    e.usage = IOHIDElementGetUsage(element);
    write(fd[1], &e, sizeof(struct KeyEvent));
}

static void open_matching_devices(struct device_properties *identifiers, io_iterator_t iter) {
    io_name_t name;
    kern_return_t kr;
    struct device_properties *current_device_properties;

    // Name of the virtual keyboard.
    CFStringRef cfKarabiner = make_cfstring("Karabiner VirtualHIDKeyboard");
    if (cfKarabiner == NULL) {
        print_iokit_error("CFStringCreateWithCString");
        return;
    }

    if (identifiers != NULL) {
        current_device_properties = (struct device_properties *)malloc(sizeof *current_device_properties);
        if (current_device_properties == NULL) {
            print_iokit_error("malloc error: device properties");
            CFRelease(cfKarabiner);
            return;
        }
        std::cout << "Grabbing keyboard: " << identifiers->product_name << "\n" << std::endl;
        // FIXME: remove print.
        print_device_properties(identifiers);
    } else {
        std::cout << "No identifiers given. Grabbing all keyboards." << std::endl;
    }


    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
        // Get product name of current iterating device.
        CFStringRef cfCurr = (CFStringRef)IORegistryEntryCreateCFProperty(curr, CFSTR(kIOHIDProductKey), kCFAllocatorDefault, kIOHIDOptionsTypeNone);
        if (cfCurr == NULL) {
            print_iokit_error("IORegistryEntryCreateCFProperty");
            continue;
        }

        bool match = CFStringCompare(cfCurr, cfKarabiner, 0) != kCFCompareEqualTo;
        CFRelease(cfCurr);
        if (!match) // Skip if virtual keyboard.
            continue;

        if (identifiers != NULL) {
            get_device_properties(curr, current_device_properties);
            // FIXME: remove print.
            print_device_properties(current_device_properties);
            match = compare_device_properties(identifiers, current_device_properties);
        }
        if (!match)
            continue;

        IOHIDDeviceRef dev = IOHIDDeviceCreate(kCFAllocatorDefault, curr);
        source_device[curr] = dev;
        IOHIDDeviceRegisterInputValueCallback(dev, input_callback, NULL);
        kr = IOHIDDeviceOpen(dev, kIOHIDOptionsTypeSeizeDevice);
        if (kr != kIOReturnSuccess) {
            print_iokit_error("IOHIDDeviceOpen", kr);
        }
        IOHIDDeviceScheduleWithRunLoop(dev, listener_loop, kCFRunLoopDefaultMode);
    }

    if (identifiers != NULL) {
        free(identifiers);
    }
    CFRelease(cfKarabiner);
}

/*
 * We'll register this callback to run whenever an IOHIDDevice
 * (representing a keyboard) is connected to the OS
 *
 */
static void matched_callback(void *context, io_iterator_t iter) {
    struct device_properties *identifiers = (struct device_properties *)context;
    open_matching_devices(identifiers, iter);
}

/*
 * We'll register this callback to run whenever an IOHIDDevice
 * (representing a keyboard) is disconnected from the OS
 *
 */
static void terminated_callback(void *context, io_iterator_t iter) {
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
        source_device.erase(curr);
    }
}

/*
 * Reads a new key event from the pipe, blocking until a new event is
 * ready.
 */
extern "C" int wait_key(struct KeyEvent *e) {
    return read(fd[0], e, sizeof(struct KeyEvent)) == sizeof(struct KeyEvent);
}

/*
 * For each keyboard, registers an asynchronous callback to run when
 * new input from the user is available from that keyboard. Then
 * sleeps indefinitely, ready to received asynchronous callbacks.
 */
static void monitor_kb(struct device_properties *identifiers) {
    kern_return_t kr;
    CFMutableDictionaryRef matching_dictionary = IOServiceMatching(kIOHIDDeviceKey);
    if(!matching_dictionary) {
        print_iokit_error("IOServiceMatching");
        return;
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
    CFRetain(matching_dictionary);
    kr = IOServiceGetMatchingServices(kIOMasterPortDefault,
                                      matching_dictionary,
                                      &iter);
    if(kr != KERN_SUCCESS) {
        print_iokit_error("IOServiceGetMatchingServices", kr);
        return;
    }
    listener_loop = CFRunLoopGetCurrent();
    open_matching_devices(identifiers, iter);
    IONotificationPortRef notification_port = IONotificationPortCreate(kIOMasterPortDefault);
    CFRunLoopSourceRef notification_source = IONotificationPortGetRunLoopSource(notification_port);
    CFRunLoopAddSource(listener_loop, notification_source, kCFRunLoopDefaultMode);
    CFRetain(matching_dictionary);
    kr = IOServiceAddMatchingNotification(notification_port,
                                          kIOMatchedNotification,
                                          matching_dictionary,
                                          matched_callback,
                                          identifiers,
                                          &iter);
    if(kr != KERN_SUCCESS) {
        print_iokit_error("IOServiceAddMatchingNotification", kr);
        return;
    }
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {}
    kr = IOServiceAddMatchingNotification(notification_port,
                                          kIOTerminatedNotification,
                                          matching_dictionary,
                                          terminated_callback,
                                          NULL,
                                          &iter);
    if(kr != KERN_SUCCESS) {
        print_iokit_error("IOServiceAddMatchingNotification", kr);
        return;
    }
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {}
    CFRunLoopRun();
    for(std::pair<const io_service_t,IOHIDDeviceRef> p: source_device) {
        kr = IOHIDDeviceClose(p.second,kIOHIDOptionsTypeSeizeDevice);
        if(kr != KERN_SUCCESS) {
            print_iokit_error("IOHIDDeviceClose", kr);
        }
    }
}

/*
 * Opens and seizes input from each keyboard device whose product name
 * matches the parameter (if NULL is received, then it opens all
 * keyboard devices). Spawns a thread to receive asynchronous input
 * and opens a pipe for this thread to send key event data to the main
 * thread.
 *
 * Loads a the karabiner kernel extension that will send key events
 * back to the OS.
 */
extern "C" int grab_kb(struct device_properties *identifiers) {
    // Source
    if (pipe(fd) == -1) {
        std::cerr << "pipe error: " << errno << std::endl;
        return errno;
    }
    thread = std::thread{monitor_kb, identifiers};
    // Sink
    return init_sink();
}

/*
 * Releases the resources needed to receive key events from and send
 * key events to the OS.
 */
extern "C" int release_kb() {
    int retval = 0;
    kern_return_t kr;
    // Source
    if(thread.joinable()) {
        CFRunLoopStop(listener_loop);
        thread.join();
    } else {
        std::cerr << "No thread was running!" << std::endl;
    }
    if (close(fd[0]) == -1) {
        std::cerr << "close error: " << errno << std::endl;
        retval = 1;
    }
    if (close(fd[1]) == -1) {
        std::cerr << "close error: " << errno << std::endl;
        retval = 1;
    }
    // Sink
    if(exit_sink()) retval = 1;
    return retval;
}
