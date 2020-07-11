#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hidsystem/IOHIDShared.h>
#include <unistd.h>
#include <thread>
#include <vector>
#include <iostream>

#include "karabiner_virtual_hid_device_methods.hpp"

struct KeyEvent {
    uint8_t type;
    uint32_t keycode;
};

// Sink
static mach_port_t connect;
static io_service_t service;
static pqrs::karabiner_virtual_hid_device::hid_report::keyboard_input keyboard;
static pqrs::karabiner_virtual_hid_device::hid_report::apple_vendor_top_case_input top_case;
static pqrs::karabiner_virtual_hid_device::hid_report::apple_vendor_keyboard_input apple_keyboard;
static pqrs::karabiner_virtual_hid_device::hid_report::consumer_input consumer;

// Source
static std::thread thread;
static CFRunLoopRef listener_loop;
static std::vector<IOHIDDeviceRef> source_device;
static int fd[2];

void callback(void *context, IOReturn result, void *sender, IOHIDValueRef value) {
    struct KeyEvent e;
    CFIndex integer_value = IOHIDValueGetIntegerValue(value);
    IOHIDElementRef element = IOHIDValueGetElement(value);
    uint16_t usage_page = IOHIDElementGetUsagePage(element);
    uint16_t usage = IOHIDElementGetUsage(element);
    e.type = !integer_value;
    e.keycode = (usage_page << 16) | usage;
    write(fd[1], &e, sizeof(struct KeyEvent));
}

template<typename T>
int send_key(T &keyboard, struct KeyEvent *e) {
    if(e->type == 0) keyboard.keys.insert((uint16_t)e->keycode);
    else if (e->type == 1) keyboard.keys.erase((uint16_t)e->keycode);
    return pqrs::karabiner_virtual_hid_device_methods::post_keyboard_input_report(connect, keyboard);
}

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

extern "C" int wait_key(struct KeyEvent *e) {
    return read(fd[0], e, sizeof(struct KeyEvent)) == sizeof(struct KeyEvent);
}

void monitor_kb() {
    listener_loop = CFRunLoopGetCurrent();
    for(IOHIDDeviceRef d : source_device) {
        IOHIDDeviceScheduleWithRunLoop(d, listener_loop, kCFRunLoopDefaultMode);
    }
    CFRunLoopRun();
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
    if(r != KERN_SUCCESS) return IO_OBJECT_NULL;
    return iter;
}

extern "C" int grab_kb(char *product) {
    // Source
    io_iterator_t iter = get_device_iterator();
    if(!iter) {
        std::cerr << "get_device_iterator failed" << std::endl;
    }
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
        if(product) {
            CFStringRef product_name = CFStringCreateWithCString(kCFAllocatorDefault, product, CFStringGetSystemEncoding());
            CFStringRef port_name = (CFStringRef)IORegistryEntryCreateCFProperty(curr, CFSTR(kIOHIDProductKey), kCFAllocatorDefault, kIOHIDOptionsTypeNone);
            bool match = CFStringCompare(product_name, port_name, 0) == kCFCompareEqualTo;
            CFRelease(product_name);
            CFRelease(port_name);
            if(!match) continue;
        }
        IOHIDDeviceRef dev = IOHIDDeviceCreate(kCFAllocatorDefault, curr);
        source_device.push_back(dev);
        IOHIDDeviceRegisterInputValueCallback(dev, callback, NULL);
        IOReturn ret = IOHIDDeviceOpen(dev, kIOHIDOptionsTypeSeizeDevice);
        if(ret == kIOReturnNotPrivileged) {
            std::cerr << "Process must have root privileges to capture keyboard input, see README" << std::endl;
            return 1;
        }
    }
    if (pipe(fd) == -1) { 
        std::cerr << "Pipe Failed" << std::endl;
        return 1; 
    }
    thread = std::thread{monitor_kb};
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
        std::cerr << "IOServiceOpen error" << std::endl;
        return 1;
    }
    {
    pqrs::karabiner_virtual_hid_device::properties::keyboard_initialization properties;
    kr = pqrs::karabiner_virtual_hid_device_methods::initialize_virtual_hid_keyboard(connect, properties);
    if (kr != KERN_SUCCESS) {
        std::cerr << "initialize_virtual_hid_keyboard error" << std::endl;
        return 1;
    }
    while (true) {
        bool ready;
        kr = pqrs::karabiner_virtual_hid_device_methods::is_virtual_hid_keyboard_ready(connect, ready);
        if (kr != KERN_SUCCESS) {
            std::cerr << "is_virtual_hid_keyboard_ready error: " << kr << std::endl;
            return 1;
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
        std::cerr << "initialize_virtual_hid_keyboard error" << std::endl;
        return 1;
    }
    }
    return 0;
}

extern "C" int release_kb() {
    // Source
    for(IOHIDDeviceRef dev : source_device) {
        IOReturn ret = IOHIDDeviceClose(dev,kIOHIDOptionsTypeSeizeDevice);
    }
    if(thread.joinable()) {
        CFRunLoopStop(listener_loop);
        thread.join();
    }
    // Sink
    kern_return_t kr = pqrs::karabiner_virtual_hid_device_methods::reset_virtual_hid_keyboard(connect);
    if (kr != KERN_SUCCESS) {
        std::cerr << "reset_virtual_hid_keyboard error" << std::endl;
    }
    if (connect) {
        IOServiceClose(connect);
    }
    if (service) {
        IOObjectRelease(service);
    }
    return 0;
}

#ifdef STANDALONE
int main() {
    io_iterator_t iter = get_device_iterator();
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
        CFTypeRef str = IORegistryEntryCreateCFProperty(curr,
                                                        CFSTR("Product"),
                                                        kCFAllocatorDefault,
                                                        kIOHIDOptionsTypeNone);
        CFShow(str);
    }

}       
#endif
