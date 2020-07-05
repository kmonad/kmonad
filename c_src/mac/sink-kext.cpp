#include "karabiner_virtual_hid_device_methods.hpp"
#include "keyevent.h"

#include <IOKit/hidsystem/IOHIDShared.h>
#include <iostream>
#include <thread>

mach_port_t connect;
io_service_t service;
pqrs::karabiner_virtual_hid_device::hid_report::keyboard_input pressed;

extern "C" int kext_post(struct KeyEvent *e) {
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
    return 0;
}

extern "C" int kext_init() {
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

extern "C" int kext_exit() {
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
