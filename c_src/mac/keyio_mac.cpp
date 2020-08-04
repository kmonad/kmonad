#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hidsystem/IOHIDShared.h>
#include <unistd.h>
#include <errno.h>
#include <thread>
#include <map>
#include <iostream>

#include "karabiner_virtual_hid_device_methods.hpp"

/*
 * Key event information that's shared between C++ and Haskell.
 *
 * type: represents key up or key down
 *
 * keycode: 16 uppermost bits represent IOKit usage page
 *          16 lowermost bits represent IOKit usage
 */
struct KeyEvent {
    uint8_t type;
    uint32_t keycode;
};

/*
 * Resources needed to post altered key events back to the OS. They
 * are global so that they can be kept track of in the C++ code rather
 * than in the Haskell.
 */
static mach_port_t connect;
static io_service_t service;
static pqrs::karabiner_virtual_hid_device::hid_report::keyboard_input keyboard;
static pqrs::karabiner_virtual_hid_device::hid_report::apple_vendor_top_case_input top_case;
static pqrs::karabiner_virtual_hid_device::hid_report::apple_vendor_keyboard_input apple_keyboard;
static pqrs::karabiner_virtual_hid_device::hid_report::consumer_input consumer;

/*
 * These are needed to receive unaltered key events from the OS.
 */
static std::thread thread;
static CFRunLoopRef listener_loop;
static std::map<io_service_t,IOHIDDeviceRef> source_device;
static int fd[2];
static char *prod = nullptr;

/*
 * We'll register this callback to run whenever an IOHIDDevice
 * (representing a keyboard) sends input from the user.
 *
 * It passes the relevant information into a pipe that will be read
 * from with wait_key.
 */
void input_callback(void *context, IOReturn result, void *sender, IOHIDValueRef value) {
    struct KeyEvent e;
    CFIndex integer_value = IOHIDValueGetIntegerValue(value);
    IOHIDElementRef element = IOHIDValueGetElement(value);
    uint16_t usage_page = IOHIDElementGetUsagePage(element);
    uint16_t usage = IOHIDElementGetUsage(element);
    e.type = !integer_value;
    e.keycode = (usage_page << 16) | usage;
    write(fd[1], &e, sizeof(struct KeyEvent));
}

void open_matching_devices(char *product, io_iterator_t iter) {
    io_name_t name;
    kern_return_t kr;
    CFStringRef cfproduct = NULL;
    if(product) {
        CFStringRef cfproduct = CFStringCreateWithCString(kCFAllocatorDefault, product, CFStringGetSystemEncoding());
        if(cfproduct == NULL) {
            std::cerr << "CFStringCreateWithCString error" << std::endl;
            return;
        }
    }
    CFStringRef cfkarabiner = CFStringCreateWithCString(kCFAllocatorDefault, "Karabiner VirtualHIDKeyboard", CFStringGetSystemEncoding());
    if(cfkarabiner == NULL) {
        std::cerr << "CFStringCreateWithCString error" << std::endl;
        if(product) {
            CFRelease(cfproduct);
        }
        return;
    }
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
        CFStringRef cfcurr = (CFStringRef)IORegistryEntryCreateCFProperty(curr, CFSTR(kIOHIDProductKey), kCFAllocatorDefault, kIOHIDOptionsTypeNone);
        if(cfcurr == NULL) {
            std::cerr << "IORegistryEntryCreateCFProperty error" << std::endl;
            CFRelease(cfcurr);
            continue;
        }
        bool match = (CFStringCompare(cfcurr, cfkarabiner, 0) != kCFCompareEqualTo);
        if(product) {
            match = match && (CFStringCompare(cfcurr, cfproduct, 0) == kCFCompareEqualTo);
        }
        CFRelease(cfcurr);
        if(!match) continue;
        IOHIDDeviceRef dev = IOHIDDeviceCreate(kCFAllocatorDefault, curr);
        source_device[curr] = dev;
        IOHIDDeviceRegisterInputValueCallback(dev, input_callback, NULL);
        kr = IOHIDDeviceOpen(dev, kIOHIDOptionsTypeSeizeDevice);
        if(kr != kIOReturnSuccess) {
            std::cerr << "IOHIDDeviceOpen error: " << std::hex << kr << std::endl;
            if(kr == kIOReturnNotPrivileged) {
                std::cerr << "IOHIDDeviceOpen requires root privileges when called with kIOHIDOptionsTypeSeizeDevice" << std::endl;
            }
        }
        IOHIDDeviceScheduleWithRunLoop(dev, listener_loop, kCFRunLoopDefaultMode);
    }
    if(product) {
        CFRelease(cfproduct);
    }
    CFRelease(cfkarabiner);
}

/*
 * We'll register this callback to run whenever an IOHIDDevice
 * (representing a keyboard) is connected to the OS
 *
 */
void matched_callback(void *context, io_iterator_t iter) {
    char *product = (char *)context;
    open_matching_devices(product, iter);
}

/*
 * We'll register this callback to run whenever an IOHIDDevice
 * (representing a keyboard) is disconnected from the OS
 *
 */
void terminated_callback(void *context, io_iterator_t iter) {
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
        source_device.erase(curr);
    }
}

/*
 * This gets us some code reuse (see the send_key overload below)
 */
template<typename T>
int send_key(T &keyboard, struct KeyEvent *e) {
    if(e->type == 0) keyboard.keys.insert((uint16_t)e->keycode);
    else if (e->type == 1) keyboard.keys.erase((uint16_t)e->keycode);
    return pqrs::karabiner_virtual_hid_device_methods::post_keyboard_input_report(connect, keyboard);
}

/*
 * Haskell calls this with a new key event to send back to the OS. It
 * posts the information to the karabiner kernel extension (which
 * represents a virtual keyboard).
 */
extern "C" int send_key(struct KeyEvent *e) {
    pqrs::karabiner_virtual_hid_device::usage_page usage_page = pqrs::karabiner_virtual_hid_device::usage_page(e->keycode >> 16);
    if(usage_page == pqrs::karabiner_virtual_hid_device::usage_page::keyboard_or_keypad)
        return send_key(keyboard, e);
    else if(usage_page == pqrs::karabiner_virtual_hid_device::usage_page::apple_vendor_top_case)
        return send_key(top_case, e);
    else if(usage_page == pqrs::karabiner_virtual_hid_device::usage_page::apple_vendor_keyboard)
        return send_key(apple_keyboard, e);
    else if(usage_page == pqrs::karabiner_virtual_hid_device::usage_page::consumer)
        return send_key(consumer, e);
    else
        return 1;
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
void monitor_kb(char *product) {
    kern_return_t kr;
    CFMutableDictionaryRef matching_dictionary = IOServiceMatching(kIOHIDDeviceKey);
    if(!matching_dictionary) {
        std::cerr << "IOServiceMatching error" << std::endl;
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
        std::cerr << "IOServiceGetMatchingServices error: " << std::hex << kr << std::endl;
        return;
    }
    listener_loop = CFRunLoopGetCurrent();
    open_matching_devices(product, iter);
    IONotificationPortRef notification_port = IONotificationPortCreate(kIOMasterPortDefault);
    CFRunLoopSourceRef notification_source = IONotificationPortGetRunLoopSource(notification_port);
    CFRunLoopAddSource(listener_loop, notification_source, kCFRunLoopDefaultMode);
    CFRetain(matching_dictionary);
    kr = IOServiceAddMatchingNotification(notification_port,
                                          kIOMatchedNotification,
                                          matching_dictionary,
                                          matched_callback,
                                          product,
                                          &iter);
    if(kr != KERN_SUCCESS) {
        std::cerr << "IOServiceAddMatchingNotification error: " << std::hex << kr << std::endl;
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
        std::cerr << "IOServiceAddMatchingNotification error: " << std::hex << kr << std::endl;
        return;
    }
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {}
    CFRunLoopRun();
    for(std::pair<const io_service_t,IOHIDDeviceRef> p: source_device) {
        kr = IOHIDDeviceClose(p.second,kIOHIDOptionsTypeSeizeDevice);
        if(kr != KERN_SUCCESS) {
            std::cerr << "IOHIDDeviceClose error: " << std::hex << kr << std::endl;
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
extern "C" int grab_kb(char *product) {
    // Source
    if (pipe(fd) == -1) {
        std::cerr << "pipe error: " << errno << std::endl;
        return errno;
    }
    if(product) {
        prod = (char *)malloc(strlen(product) + 1);
        strcpy(prod, product);
    }
    thread = std::thread{monitor_kb, prod};
    // Sink
    kern_return_t kr;
    connect = IO_OBJECT_NULL;
    service = IOServiceGetMatchingService(kIOMasterPortDefault, IOServiceNameMatching(pqrs::karabiner_virtual_hid_device::get_virtual_hid_root_name()));
    if (!service) {
        std::cerr << "IOServiceGetMatchingService error" << std::endl;
        return 1;
    }
    kr = IOServiceOpen(service, mach_task_self(), kIOHIDServerConnectType, &connect);
    if (kr != KERN_SUCCESS) {
        std::cerr << "IOServiceOpen error: " << std::hex << kr << std::endl;
        return kr;
    }
    //std::this_thread::sleep_for(std::chrono::milliseconds(10000));
    //setuid(501);
    {
        pqrs::karabiner_virtual_hid_device::properties::keyboard_initialization properties;
        kr = pqrs::karabiner_virtual_hid_device_methods::initialize_virtual_hid_keyboard(connect, properties);
        if (kr != KERN_SUCCESS) {
            std::cerr << "initialize_virtual_hid_keyboard error: " << std::hex << kr << std::endl;
            return 1;
        }
        while (true) {
            bool ready;
            kr = pqrs::karabiner_virtual_hid_device_methods::is_virtual_hid_keyboard_ready(connect, ready);
            if (kr != KERN_SUCCESS) {
                std::cerr << "is_virtual_hid_keyboard_ready error: " << std::hex << kr << std::endl;
                return kr;
            } else {
                if (ready) {
                    break;
                }
            }
            std::this_thread::sleep_for(std::chrono::milliseconds(100));
        }
    }
    {
        pqrs::karabiner_virtual_hid_device::properties::keyboard_initialization properties;
        properties.country_code = 33;
        kr = pqrs::karabiner_virtual_hid_device_methods::initialize_virtual_hid_keyboard(connect, properties);
        if (kr != KERN_SUCCESS) {
            std::cerr << "initialize_virtual_hid_keyboard error: " << std::hex << kr << std::endl;
            return kr;
        }
    }
    return 0;
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
        std::cerr << "no thread was running!" << std::endl;
    }
    if(prod) {
        free(prod);
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
    kr = pqrs::karabiner_virtual_hid_device_methods::reset_virtual_hid_keyboard(connect);
    if (kr != KERN_SUCCESS) {
        std::cerr << "reset_virtual_hid_keyboard error: " << std::hex << kr << std::endl;
        retval = 1;
    }
    if (connect) {
        kr = IOServiceClose(connect);
        if(kr != KERN_SUCCESS) {
            std::cerr << "IOServiceClose error: " << std::hex << kr << std::endl;
            retval = 1;
        }
    }
    if (service) {
        kr = IOObjectRelease(service);
        if(kr != KERN_SUCCESS) {
            std::cerr << "IOObjectRelease error: " << std::hex << kr << std::endl;
            retval = 1;
        }
    }
    return retval;
}
