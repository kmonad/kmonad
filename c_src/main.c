// https://opensource.apple.com/source/IOHIDFamily/IOHIDFamily-503.1.13/tools/IOHIDReportTest.c.auto.html

#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hid/IOHIDManager.h>
#include <IOKit/IOKitLib.h>

static bool     gReport         = FALSE;
static bool     gValue          = TRUE;
static bool     gSend           = TRUE;

static char * getReportTypeString(IOHIDReportType type)
{
    switch ( type ) {
        case kIOHIDReportTypeInput:
            return "INPUT";
        case kIOHIDReportTypeOutput:
            return "OUTPUT";
        case kIOHIDReportTypeFeature:
            return "FEATURE";
        default:
            return "DUH";
    }
}

static void __deviceReportCallback(void * context, IOReturn result, void * sender, IOHIDReportType type, uint32_t reportID, uint8_t * report, CFIndex reportLength)
{
    int index;

    printf("IOHIDDeviceRef[%p]: reportType=%s reportID=%d reportLength=%ld: ", sender, getReportTypeString(type), reportID, reportLength);
    for (index=0; index<reportLength; index++)
        printf("%02x ", report[index]);
    printf("\n");
    
    // toggle a report
    if ( gSend ) {
        static uint8_t value = 0;
        IOReturn ret = IOHIDDeviceSetReport((IOHIDDeviceRef)sender, kIOHIDReportTypeOutput, 0, &(value), 1);
        value = value+1 % 2;
        printf("Attempt to send data byte. Ret = %d\n", ret);   
    }
}

void __deviceValueCallback (void * context, IOReturn result, void * sender, IOHIDValueRef value)
{
    IOHIDElementRef element = IOHIDValueGetElement(value);
    
    if(IOHIDElementGetUsage(element) == 0xFFFFFFFF ||
       IOHIDElementGetCookie(element) == 30) return;
    printf("IOHIDDeviceRef[%p]: value=%p timestamp=%lld cookie=%d usagePage=0x%02X usage=0x%02X intValue=%ld\n", sender, value, IOHIDValueGetTimeStamp(value), (uint32_t)IOHIDElementGetCookie(element), IOHIDElementGetUsagePage(element), IOHIDElementGetUsage(element), IOHIDValueGetIntegerValue(value));
}


static void __deviceCallback(void * context, IOReturn result, void * sender, IOHIDDeviceRef device)
{        
    printf("IOHIDDeviceRef[%p] %s\n", device, context!=0 ? "matched" : "terminated");
}

int main (int argc, const char * argv[]) {

    IOHIDManagerRef manager = IOHIDManagerCreate(kCFAllocatorDefault, 0);
    int a;
    for (a=1; a<argc; a++) {
        if ( 0 == strcmp("-v", argv[a]) ) {
            gValue = TRUE;
            printf("Print values supported\n");
        }
        else if ( 0 == strcmp("-s", argv[a]) ) {
            gSend = TRUE;
            printf("Send response out data supported\n");
        }
        else if ( 0 == strcmp("-nr", argv[a]) ) {
            gReport = FALSE;
            printf("Input report data suppressed\n");
        }
    }
    
    CFMutableDictionaryRef matching_dictionary = IOServiceMatching(kIOHIDDeviceKey);
    UInt32 value;
    CFNumberRef cfValue;
    value = kHIDPage_GenericDesktop;
    cfValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberSInt32Type, &value );
    CFDictionarySetValue(matching_dictionary,
                         CFSTR(kIOHIDDeviceUsagePageKey),
                         cfValue);
    CFRelease(cfValue);
    value = kHIDUsage_GD_Keyboard;
    cfValue = CFNumberCreate( kCFAllocatorDefault, kCFNumberSInt32Type, &value );
    CFDictionarySetValue(matching_dictionary,
                         CFSTR(kIOHIDDeviceUsageKey),
                         cfValue);
    CFRelease(cfValue);


    
    //IOHIDDeviceRef device = IOHIDDeviceCreate(kCFAllocatorDefault, service);
    
    /*
      io_iterator_t it = IO_OBJECT_NULL;
      kern_return_t r = IOServiceGetMatchingServices(kIOMasterPortDefault,
      matching_dictionary,
      &it);
      printf("%d\n",r);
    
      //uint64_t id;
      //r = IORegistryEntryGetRegistryEntryID(it, &id);
    
      // Increment `it' with IOIteratorNext
      */
    io_service_t service = 0;
    IOHIDDeviceRef device = 0;
    service = IOServiceGetMatchingService(kIOMasterPortDefault,
                                          matching_dictionary);
    device = IOHIDDeviceCreate(kCFAllocatorDefault, service);    
    IOReturn ret = IOHIDDeviceOpen(device,kIOHIDOptionsTypeNone);
    printf("%x\n",ret);

    //IOHIDDeviceGetProperty();
    io_name_t devName;
    IORegistryEntryGetName(service, devName);
    printf("Device's name = %s\n", devName);
    IOHIDQueueRef queue = IOHIDQueueCreate(kCFAllocatorDefault,
                                           device,
                                           32,
                                           kIOHIDOptionsTypeNone);
    CFArrayRef elements = IOHIDDeviceCopyMatchingElements(device,
                                                          NULL,
                                                          kIOHIDOptionsTypeNone);

    printf("%ld\n",CFArrayGetCount(elements));
    for (CFIndex i = 0; i < CFArrayGetCount(elements); ++i) {
        IOHIDQueueAddElement(queue,
                             (IOHIDElementRef)CFArrayGetValueAtIndex(elements, i));
    }
    CFRelease(elements);

    IOHIDQueueStart(queue);
    while(1) {
        IOHIDValueRef value = IOHIDQueueCopyNextValue(queue);
        if(value)
            printf("%p\n",value);
    }

    /*
    IOHIDManagerSetDeviceMatching(manager,matching_dictionary);
    
    // CFRelease(matching_dictionary);

    IOHIDManagerRegisterDeviceMatchingCallback(manager, __deviceCallback, (void*)TRUE);
    IOHIDManagerRegisterDeviceRemovalCallback(manager, __deviceCallback, (void*)FALSE);
    
    if ( gReport )
        IOHIDManagerRegisterInputReportCallback(manager, __deviceReportCallback, NULL);
    if ( gValue ) 
        IOHIDManagerRegisterInputValueCallback(manager, __deviceValueCallback, NULL);
    
    IOHIDManagerScheduleWithRunLoop(manager, CFRunLoopGetCurrent(), kCFRunLoopDefaultMode);
    // IOHIDManagerSetDeviceMatching(manager, NULL);
    IOHIDManagerOpen(manager, 0);
    
    CFRunLoopRun();
    
    return 0;
    */
}
