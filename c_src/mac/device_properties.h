#include <stdbool.h>
#include <IOKit/hid/IOHIDLib.h>
#include <stdint.h>

struct device_properties {
    char *product_name;
    char *manufacturer;
    char *serial_number;
    char *transport;

    int64_t country_code;
    int64_t location_id;
    int64_t product_id;
    int64_t vendor_id;
    int64_t version_number;

    // bool is_built_in;
};

void get_device_properties(mach_port_t _device, struct device_properties *properties);
void print_device_properties(struct device_properties *properties);
bool compare_device_properties(struct device_properties *props1,
                               struct device_properties *prop2);
