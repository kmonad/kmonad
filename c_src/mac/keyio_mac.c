#include "sink-kext.h"
#include "sink-iokit.h"
#include "source-hidqueue.h"
#include "hid.h"
#include "keyevent.h"

static IOHIDDeviceRef device;

static int (*sink_post)(struct KeyEvent *e);
static int (*sink_init)();
static int (*sink_exit)();

void send_key(struct KeyEvent *e) {
    sink_post(e);
}

void wait_key(struct KeyEvent *e) {
    hid_queue_read(e);
}

int grab_kb() {
    device = device_open();
    hid_queue_open(device);
    sink_init();
    return 0;
}

int release_kb() {
    device_close(device);
    hid_queue_close();
    sink_exit();
    return 0;
}

#ifdef STANDALONE
int main(int argc, char **argv) {
    int c;
    while ((c = getopt (argc, argv, "o:")) != -1) {
        switch (c) {
        case 'o':
            printf("Sink: ");
            if(0 == strcmp(optarg,"kext")) {
                printf("kext\n");
                sink_post = &kext_post;
                sink_init = kext_init;
                sink_exit = kext_exit;
            } else if(0 == strcmp(optarg,"iokit")) {
                printf("iokit\n");
                sink_post = iokit_post;
                sink_init = iokit_init;
                sink_exit = iokit_exit;
            }
            break;
        }
    }
    grab_kb();
    struct KeyEvent ke;
    ke.keycode = 0;
    while(ke.keycode != 0x3) {
        printf("%d\n",ke.keycode);
        wait_key(&ke);
        send_key(&ke);
    }
    release_kb();
}       
#endif
