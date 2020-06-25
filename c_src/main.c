// https://opensource.apple.com/source/IOHIDFamily/IOHIDFamily-503.1.13/tools/IOHIDReportTest.c.auto.html

#include <CoreFoundation/CoreFoundation.h>
#include <IOKit/hid/IOHIDLib.h>
#include <IOKit/hid/IOHIDManager.h>
#include <IOKit/IOKitLib.h>

static bool     gReport         = TRUE;
static bool     gValue          = FALSE;
static bool     gSend           = FALSE;

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
}
