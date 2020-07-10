#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hidsystem/IOHIDShared.h>
#include <unistd.h>
#include <pthread.h>
#include "sink-kext.h"
#include "keyevent.h"

int keymap[] = { 0x0,
                 0x0,
                 0x0,
                 0x0,
                 0x0,
                 0xb,
                 0x8,
                 0x2,
                 0xe,
                 0x3,
                 0x5,
                 0x4,
                 0x22,
                 0x26,
                 0x28,
                 0x25,
                 0x2e,
                 0x2d,
                 0x1f,
                 0x23,
                 0xc,
                 0xf,
                 0x1,
                 0x11,
                 0x20,
                 0x9,
                 0xd,
                 0x7,
                 0x10,
                 0x6,
                 0x12,
                 0x13,
                 0x14,
                 0x15,
                 0x17,
                 0x16,
                 0x1a,
                 0x1c,
                 0x19,
                 0x1d,
                 0x24,
                 0x35,
                 0x33,
                 0x30,
                 0x31,
                 0x1b,
                 0x18,
                 0x21,
                 0x1e,
                 0x2a,
                 0x2a, // non_us_pound == backslash
                 0x29,
                 0x27,
                 0x32,
                 0x2b,
                 0x2f,
                 0x2c,
                 0x39,
                 0x7a,
                 0x78,
                 0x63,
                 0x76,
                 0x60,
                 0x61,
                 0x62,
                 0x64,
                 0x65,
                 0x6d,
                 0x67,
                 0x6f,
                 0x69,
                 0x6b,
                 0x71,
                 0x72,
                 0x73,
                 0x74,
                 0x75,
                 0x77,
                 0x79,
                 0x7c,
                 0x7b,
                 0x7d,
                 0x7e,
                 0x47,
                 0x4b,
                 0x43,
                 0x4e,
                 0x45,
                 0x4c,
                 0x53,
                 0x54,
                 0x55,
                 0x56,
                 0x57,
                 0x58,
                 0x59,
                 0x5b,
                 0x5c,
                 0x52,
                 0x41,
                 0xa,
                 0x6e,
                 0x0, // keyboard_power => aux_control_button
                 0x51,
                 0x69,
                 0x6b,
                 0x71,
                 0x6a,
                 0x40,
                 0x4f,
                 0x50,
                 0x5a,
                 0x0, // keyboard_f21
                 0x0, // keyboard_f22
                 0x0, // keyboard_f23
                 0x0, // keyboard_f24
                 0x0, // keyboard_execute
                 0x72,
                 0x0, // keyboard_menu
                 0x0, // keyboard_select
                 0x0, // keyboard_stop
                 0x0, // keyboard_again
                 0x0, // keyboard_undo
                 0x0, // keyboard_cut
                 0x0, // keyboard_copy
                 0x0, // keyboard_paste
                 0x0, // keyboard_find
                 0x0, // keyboard_mute => aux_control_button
                 0x0, // keyboard_volume_up => aux_control_button
                 0x0, // keyboard_volume_down => aux_control_button
                 0x0, // keyboard_locking_caps_lock
                 0x0, // keyboard_locking_num_lock
                 0x0, // keyboard_locking_scroll_lock
                 0x5f,
                 0x0, // keypad_equal_sign_as400
                 0x5e,
                 0x0, // keyboard_international2
                 0x5d,
                 0x0, // keyboard_international4
                 0x0, // keyboard_international5
                 0x0, // keyboard_international6
                 0x0, // keyboard_international7
                 0x0, // keyboard_international8
                 0x0, // keyboard_international9
                 0x68,
                 0x66,
                 0x0, // keyboard_lang3
                 0x0, // keyboard_lang4
                 0x0, // keyboard_lang5
                 0x0, // keyboard_lang6
                 0x0, // keyboard_lang7
                 0x0, // keyboard_lang8
                 0x0, // keyboard_lang9
                 0x0, // keyboard_alternate_erase
                 0x0, // keyboard_sys_req_or_attention
                 0x0, // keyboard_cancel
                 0x0, // keyboard_clear
                 0x0, // keyboard_prior
                 0x0, // keyboard_return
                 0x0, // keyboard_separator
                 0x0, // keyboard_out
                 0x0, // keyboard_oper
                 0x0, // keyboard_clear_or_again
                 0x0, // keyboard_cr_sel_or_props
                 0x0, // keyboard_ex_sel
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x0, // reserved
                 0x3b,
                 0x38,
                 0x3a,
                 0x37,
                 0x3e,
                 0x3c,
                 0x3d,
                 0x36 };

static mach_port_t sink;
static pthread_t thread_id;
static IOHIDDeviceRef source_device;
static int fd[2];

/*
static int apple_keycodes = { 10,
                              10,
                              10};
*/

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
    /*
    if((usage_page == kHIDPage_KeyboardOrKeypad &&
        kHIDUsage_KeyboardErrorUndefined < usage &&
        usage < kHIDUsage_Keyboard_Reserved) ||
       (usage_page == 0xff)) { // See AppleHIDUsageTables.h
        e.type = !integer_value;
        e.keycode = usage;
        write(fd[1], &e, sizeof(struct KeyEvent));
    }    
    */
}

int send_key(struct KeyEvent *e) {
    kext_post(e);
    /*
    int action;
    IOOptionBits flags;
    IOOptionBits options;
    IOGPoint loc;
    NXEventData event;
    bzero(&event, sizeof(NXEventData));
        //printf("%x\n",event.key.keyCode);
    if(e->keycode >= 0xE0) {
        action = NX_FLAGSCHANGED;
        flags = NX_SHIFTMASK;//e->type? 0 : 0x40001;
        options = 1;
    } else {
        event.key.keyCode = keymap[e->keycode];
        event.key.origCharSet = event.key.charSet = NX_ASCIISET;
        event.key.origCharCode = event.key.charCode = 0;
        action = e->type? NX_KEYUP : NX_KEYDOWN;
        flags = kIOHIDOptionsTypeNone;
        options = kIOHIDSetGlobalEventFlags;
    }
    IOReturn kr = IOHIDPostEvent(sink, action,
                                 loc, &event, kNXEventDataVersion,
                                 flags, kIOHIDSetGlobalEventFlags);
    */
    return 0;
}

int wait_key(struct KeyEvent *e) {
    read(fd[0], e, sizeof(struct KeyEvent));
    return 0;
}

void *monitor_kb(void *ctx) {
    IOHIDDeviceScheduleWithRunLoop(source_device, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
    CFRunLoopRun();
    return NULL;
}

void list_kb() {
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
                                                   matching_dictionary, //IOServiceMatching(kIOHIKeyboardClass)
                                                   &iter);
    for(mach_port_t curr = IOIteratorNext(iter); curr; curr = IOIteratorNext(iter)) {
        /*
        io_name_t devName;
        IORegistryEntryGetName(curr, devName);
        printf("Device's name = %s\n", devName);
        */
        CFTypeRef str = IORegistryEntryCreateCFProperty(curr, CFSTR("Product"), kCFAllocatorDefault, kIOHIDOptionsTypeNone);
        CFShow(str);
    }
}

// product can be empty, in which case any keyboard is chosen
int grab_kb(char *product) {
    // Source
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
                                                   matching_dictionary, //IOServiceMatching(kIOHIKeyboardClass)
                                                   &iter);
    mach_port_t service = IOIteratorNext(iter);
    if(product) {
        printf("Got: %s\n",product);
        // This should be achieved in the matching dictionary, but wouldn't work]
        CFStringRef cf_product = CFStringCreateWithCString(kCFAllocatorDefault, product, CFStringGetSystemEncoding());
        while(service) {
            //io_name_t devName;
            //IORegistryEntryGetName(curr, devName);
            //printf("Device's name = %s\n", devName);
            CFTypeRef cf_curr_product = IORegistryEntryCreateCFProperty(service, CFSTR(kIOHIDProductKey), kCFAllocatorDefault, kIOHIDOptionsTypeNone);
            /*
            if(CFGetTypeID(cf_curr_product) == CFStringGetTypeID()) printf("string");
            else if(CFGetTypeID(cf_curr_product) == CFNumberGetTypeID()) printf("num");
            else printf("other");
            int x;
            printf("success? %d\n",CFNumberGetValue(cf_curr_product, CFNumberGetType(cf_curr_product), &x));
            printf("%d\n",x);
            */
            if(CFStringCompare(cf_product, cf_curr_product, 0) == kCFCompareEqualTo) {
                break;
            }
            service = IOIteratorNext(iter);
        }
    }
    source_device = IOHIDDeviceCreate(kCFAllocatorDefault, service);
    IOHIDDeviceRegisterInputValueCallback(source_device, callback, NULL);
    if (pipe(fd) == -1) { 
        fprintf(stderr, "Pipe Failed" ); 
        return 1; 
    }
    IOReturn ret = IOHIDDeviceOpen(source_device,kIOHIDOptionsTypeSeizeDevice);
    if(ret == kIOReturnNotPrivileged) {
        printf("Run as root\n");
    }
    pthread_create(&thread_id, NULL, monitor_kb, NULL);
    // Sink
    /*
    service = IOServiceGetMatchingService(kIOMasterPortDefault, IOServiceMatching(kIOHIDSystemClass));
    IOReturn r = IOServiceOpen(service, mach_task_self(), kIOHIDParamConnectType, &sink);
    */
    kext_init();
    return 0;
}

int release_kb() {
    // Source
    IOReturn ret = IOHIDDeviceClose(source_device,kIOHIDOptionsTypeSeizeDevice);
    pthread_cancel(thread_id);
    // Sink
    // kern_return_t r = IOServiceClose(sink);
    kext_exit();
    return 0;
}

#ifdef STANDALONE
int main() {
    /*
    grab_kb("Apple Internal Keyboard / Trackpad kkkdkdkdkd");
    setuid(501);
    struct KeyEvent ke;
    ke.keycode = 0;
    while(ke.keycode != 0x4) {
        wait_key(&ke);
        send_key(&ke);
    }
    release_kb();
    */
    list_kb();
}       
#endif
