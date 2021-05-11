#include "keyio_mac.hpp"

#include "karabiner_virtual_hid_device_methods.hpp"

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

int exit_sink() {
    int retval = 0;
    kern_return_t kr = pqrs::karabiner_virtual_hid_device_methods::reset_virtual_hid_keyboard(connect);
    if (kr != KERN_SUCCESS) {
        print_iokit_error("reset_virtual_hid_keyboard", kr);
        retval = 1;
    }
    if (connect) {
        kr = IOServiceClose(connect);
        if(kr != KERN_SUCCESS) {
            print_iokit_error("IOServiceClose", kr);
            retval = 1;
        }
    }
    if (service) {
        kr = IOObjectRelease(service);
        if(kr != KERN_SUCCESS) {
            print_iokit_error("IOObjectRelease", kr);
            retval = 1;
        }
    }
    return retval;
}

int init_sink() {
    kern_return_t kr;
    connect = IO_OBJECT_NULL;
    service = IOServiceGetMatchingService(kIOMasterPortDefault, IOServiceNameMatching(pqrs::karabiner_virtual_hid_device::get_virtual_hid_root_name()));
    if (!service) {
        print_iokit_error("IOServiceGetMatchingService");
        return 1;
    }
    kr = IOServiceOpen(service, mach_task_self(), kIOHIDServerConnectType, &connect);
    if (kr != KERN_SUCCESS) {
        print_iokit_error("IOServiceOpen", kr);
        return kr;
    }
    //std::this_thread::sleep_for(std::chrono::milliseconds(10000));
    //setuid(501);
    {
        pqrs::karabiner_virtual_hid_device::properties::keyboard_initialization properties;
        kr = pqrs::karabiner_virtual_hid_device_methods::initialize_virtual_hid_keyboard(connect, properties);
        if (kr != KERN_SUCCESS) {
            print_iokit_error("initialize_virtual_hid_keyboard", kr);
            return 1;
        }
        while (true) {
            bool ready;
            kr = pqrs::karabiner_virtual_hid_device_methods::is_virtual_hid_keyboard_ready(connect, ready);
            if (kr != KERN_SUCCESS) {
                print_iokit_error("is_virtual_hid_keyboard_ready", kr);
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
            print_iokit_error("initialize_virtual_hid_keyboard", kr);
            return kr;
        }
    }
    return 0;
}

/*
 * This gets us some code reuse (see the send_key overload below)
 */
template<typename T>
int send_key(T &keyboard, struct KeyEvent *e) {
    if(e->type == 1) keyboard.keys.insert(e->usage);
    else if(e->type == 0) keyboard.keys.erase(e->usage);
    else return 1;
    return pqrs::karabiner_virtual_hid_device_methods::post_keyboard_input_report(connect, keyboard);
}

/*
 * Haskell calls this with a new key event to send back to the OS. It
 * posts the information to the karabiner kernel extension (which
 * represents a virtual keyboard).
 */
extern "C" int send_key(struct KeyEvent *e) {
    auto usage_page = pqrs::karabiner_virtual_hid_device::usage_page(e->page);
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
