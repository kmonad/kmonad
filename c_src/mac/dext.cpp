#include <filesystem> // Include this before virtual_hid_device_service.hpp to avoid compile error

#include "keyio_mac.hpp"

#include "virtual_hid_device_driver.hpp"
#include "virtual_hid_device_service.hpp"

/*
 * Resources needed to post altered key events back to the OS. They
 * are global so that they can be kept track of in the C++ code rather
 * than in the Haskell.
 */
static pqrs::karabiner::driverkit::virtual_hid_device_service::client *client;
static pqrs::karabiner::driverkit::virtual_hid_device_driver::hid_report::keyboard_input keyboard;
static pqrs::karabiner::driverkit::virtual_hid_device_driver::hid_report::apple_vendor_top_case_input top_case;
static pqrs::karabiner::driverkit::virtual_hid_device_driver::hid_report::apple_vendor_keyboard_input apple_keyboard;
static pqrs::karabiner::driverkit::virtual_hid_device_driver::hid_report::consumer_input consumer;

int init_sink() {
    pqrs::dispatcher::extra::initialize_shared_dispatcher();
    client = new pqrs::karabiner::driverkit::virtual_hid_device_service::client();
    auto copy = client;
    client->async_driver_loaded();
    client->async_driver_version_matched();
    client->async_virtual_hid_keyboard_ready();
    client->async_virtual_hid_pointing_ready();
    /**/
    client->connected.connect([copy] {
                                  std::cout << "connected" << std::endl;
                                  copy->async_virtual_hid_keyboard_initialize(pqrs::hid::country_code::us);
                              });
    client->connect_failed.connect([](auto&& error_code) {
                                       std::cout << "connect_failed " << error_code << std::endl;
                                   });
    client->closed.connect([] {
                               std::cout << "closed" << std::endl;
                           });
    client->error_occurred.connect([](auto&& error_code) {
                                       std::cout << "error_occurred " << error_code << std::endl;
                                   });
    client->driver_loaded_response.connect([](auto&& driver_loaded) {
                                               static std::optional<bool> previous_value;

                                               if (previous_value != driver_loaded) {
                                                   std::cout << "driver_loaded " << driver_loaded << std::endl;
                                                   previous_value = driver_loaded;
                                               }
                                           });
    client->driver_version_matched_response.connect([](auto&& driver_version_matched) {
                                                        static std::optional<bool> previous_value;
                                                        if (previous_value != driver_version_matched) {
                                                            std::cout << "driver_version_matched " << driver_version_matched << std::endl;
                                                            previous_value = driver_version_matched;
                                                        }
                                                    });
    /**/
    client->async_start();
    return 0;
}

int exit_sink() {
    free(client);
    pqrs::dispatcher::extra::terminate_shared_dispatcher();
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
    client->async_post_report(keyboard);
    return 0;
}


/*
 * Haskell calls this with a new key event to send back to the OS. It
 * posts the information to the karabiner kernel extension (which
 * represents a virtual keyboard).
 */
extern "C" int send_key(struct KeyEvent *e) {
    auto usage_page = pqrs::hid::usage_page::value_t(e->page);
    if(usage_page == pqrs::hid::usage_page::keyboard_or_keypad)
        return send_key(keyboard, e);
    else if(usage_page == pqrs::hid::usage_page::apple_vendor_top_case)
        return send_key(top_case, e);
    else if(usage_page == pqrs::hid::usage_page::apple_vendor_keyboard)
        return send_key(apple_keyboard, e);
    else if(usage_page == pqrs::hid::usage_page::consumer)
        return send_key(consumer, e);
    else
        return 1;
}
