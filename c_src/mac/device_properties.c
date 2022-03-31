#include <IOKit/IOKitLib.h> // IORegistryEntryCreateCFProperty
#include <stdlib.h>
#include <string.h>

#include "device_properties.h"

/* Get a number value out of a CFType object. */
bool get_number(CFTypeRef value, int64_t *varPtr)
{
    return CFNumberGetValue((CFNumberRef) value, kCFNumberSInt64Type, varPtr);
}

/* Get a string value out of a CFType object. */
char *get_cstring(CFTypeRef value)
{
    return CFStringGetCStringPtr((CFStringRef) value, kCFStringEncodingUTF8);
}

/* Get a property of the device. */
CFTypeRef get_property_by_ioreg(mach_port_t device, CFStringRef property)
{
    return IORegistryEntryCreateCFProperty(
            device,
            property,
            kCFAllocatorDefault,
            kIOHIDOptionsTypeNone);
}

#define GET_NUMBER_PROPERTY(key, var) get_number(get_property_by_ioreg(_device, CFSTR(key)), var)
#define GET_STRING_PROPERTY(key) get_cstring(get_property_by_ioreg(_device, CFSTR(key)))

void
get_device_properties(mach_port_t _device, struct device_properties *properties)
{
    properties->product_name = GET_STRING_PROPERTY(kIOHIDProductKey);
    properties->transport = GET_STRING_PROPERTY(kIOHIDTransportKey);
    properties->serial_number = GET_STRING_PROPERTY(kIOHIDSerialNumberKey);
    properties->manufacturer = GET_STRING_PROPERTY(kIOHIDManufacturerKey);

    GET_NUMBER_PROPERTY(kIOHIDCountryCodeKey, &properties->country_code);
    GET_NUMBER_PROPERTY(kIOHIDLocationIDKey, &properties->location_id);
    GET_NUMBER_PROPERTY(kIOHIDProductIDKey, &properties->product_id);
    GET_NUMBER_PROPERTY(kIOHIDVendorIDKey, &properties->vendor_id);
    GET_NUMBER_PROPERTY(kIOHIDVersionNumberKey, &properties->version_number);
}

#define PRINT_STRING_PROPERTY(PROP) if(properties->PROP != NULL) printf(#PROP": %s\n", properties->PROP)
#define PRINT_NUMBER_PROPERTY(PROP) if(properties->PROP != 0) printf(#PROP": %lld\n", properties->PROP)

void print_device_properties(struct device_properties *properties)
{
    if (properties == NULL)
        return;
    
    printf("=========================\n");
    PRINT_STRING_PROPERTY(product_name);
    PRINT_STRING_PROPERTY(manufacturer);
    PRINT_STRING_PROPERTY(serial_number);
    PRINT_STRING_PROPERTY(transport);
    printf("----------\n");
    PRINT_NUMBER_PROPERTY(product_id);
    PRINT_NUMBER_PROPERTY(vendor_id);
    PRINT_NUMBER_PROPERTY(location_id);
    PRINT_NUMBER_PROPERTY(country_code);
    PRINT_NUMBER_PROPERTY(version_number);
    printf("=========================\n");

    // printf("is_built_in: %s\n", properties.is_built_in ? "true" : "false");
}

/* `str1` has higher priority - NULL or "" means empty. */
bool compare_string_props(const char *str1, const char *str2)
{
    if (str1 == NULL || str1[0] == '\0')
        return true;

    if (str2 == NULL)
        return false;

    return strcmp(str1, str2) == 0;
}

/* `num1` has higher priority - 0 means empty. */
bool compare_number_props(const int64_t num1, const int64_t num2)
{
    if (num1 == 0)
        return true;

    return num1 == num2;
}

#define COMPARE_STRING_PROPS(PROP) (compare_string_props(props1->PROP, props2->PROP))
#define COMPARE_NUMBER_PROPS(PROP) (compare_number_props(props1->PROP, props2->PROP))

bool
compare_device_properties(struct device_properties *props1,
                          struct device_properties *props2)
{
    if (props1 == NULL)
        return true;

    if (props2 == NULL)
        return false;

    bool match = COMPARE_STRING_PROPS(product_name)  ;
    match = match && COMPARE_STRING_PROPS(manufacturer)  ;
    match = match && COMPARE_STRING_PROPS(serial_number) ;
    match = match && COMPARE_STRING_PROPS(transport);
    match = match && \
            COMPARE_NUMBER_PROPS(product_id)     && \
            COMPARE_NUMBER_PROPS(vendor_id)      && \
            COMPARE_NUMBER_PROPS(location_id)    && \
            COMPARE_NUMBER_PROPS(country_code)   && \
            COMPARE_NUMBER_PROPS(version_number);

    return match;
}
