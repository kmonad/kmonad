#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/IOKitLib.h>
#include <IOKit/hidsystem/IOHIDShared.h>
#include <float.h>
#include <iostream>
#include <thread>

#include "karabiner_virtual_hid_device_methods.hpp"

IOHIDQueueRef queue;
IOHIDDeviceRef device;
mach_port_t connect;
io_service_t service;
pqrs::karabiner_virtual_hid_device::hid_report::keyboard_input pressed;

struct KeyEvent {
    uint8_t type; // 0 is press, 1 is release
    uint32_t keycode;
};

// this and wait_key could (should?) return errors...
extern "C" void send_key(struct KeyEvent *e) {
    int key = e->keycode;
    if(e->type == 0) {
        if(pressed.keys.exists(key)) {
            std::cout << "already pressed, hmm...\n";
        } else {
            pressed.keys.insert(e->keycode);
        }
    }
    if (e->type == 1) {
        if(pressed.keys.exists(key)) {
            pressed.keys.erase(key);
        } else {
            std::cout << "never pressed, hmm...\n";
        }
    }
    kern_return_t kr = pqrs::karabiner_virtual_hid_device_methods::post_keyboard_input_report(connect, pressed);
    if (kr != KERN_SUCCESS) {
        std::cout << "post_keyboard_input_report error" << std::endl;
    }
}

extern "C" void wait_key(struct KeyEvent *e) {
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

extern "C" int grab_kb() {
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


    kern_return_t kr;
    connect = IO_OBJECT_NULL;
    service = IOServiceGetMatchingService(kIOMasterPortDefault, IOServiceNameMatching(pqrs::karabiner_virtual_hid_device::get_virtual_hid_root_name()));
    if (!service) {
        std::cerr << "IOServiceGetMatchingService error" << std::endl;
        //goto finish;
    }

    kr = IOServiceOpen(service, mach_task_self(), kIOHIDServerConnectType, &connect);
    if (kr != KERN_SUCCESS) {
        std::cerr << "IOServiceOpen error" << std::endl;
        //goto finish;
    }

    {
        pqrs::karabiner_virtual_hid_device::properties::keyboard_initialization properties;
        kr = pqrs::karabiner_virtual_hid_device_methods::initialize_virtual_hid_keyboard(connect, properties);
        if (kr != KERN_SUCCESS) {
            std::cerr << "initialize_virtual_hid_keyboard error" << std::endl;
        }

        while (true) {
            std::cout << "Checking virtual_hid_keyboard is ready..." << std::endl;

            bool ready;
            kr = pqrs::karabiner_virtual_hid_device_methods::is_virtual_hid_keyboard_ready(connect, ready);
            if (kr != KERN_SUCCESS) {
                std::cerr << "is_virtual_hid_keyboard_ready error: " << kr << std::endl;
            } else {
                if (ready) {
                    std::cout << "virtual_hid_keyboard is ready." << std::endl;
                    break;
                }
            }

            std::this_thread::sleep_for(std::chrono::milliseconds(100));
        }
    }

    // keyboard_input

    {
        pqrs::karabiner_virtual_hid_device::properties::keyboard_initialization properties;
#if 0
        properties.country_code = 33;
#endif
        kr = pqrs::karabiner_virtual_hid_device_methods::initialize_virtual_hid_keyboard(connect, properties);
        if (kr != KERN_SUCCESS) {
            std::cerr << "initialize_virtual_hid_keyboard error" << std::endl;
        }
        std::this_thread::sleep_for(std::chrono::milliseconds(500));
    }

    return 0;
}

extern "C" int release_kb() {
    IOHIDQueueStop(queue);
    IOReturn ret = IOHIDDeviceClose(device,kIOHIDOptionsTypeSeizeDevice);

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
    grab_kb();
    struct KeyEvent ke;
    while(1) {
        wait_key(&ke);
        send_key(&ke);
    }
}       
#endif
