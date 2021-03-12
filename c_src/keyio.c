#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <linux/input.h>
#include <linux/uinput.h>
#include <fcntl.h>
#include <poll.h>
#include <errno.h>
#include <pthread.h>

#define test_bit(bit, array) ((array[bit/8] & (1 << bit%8)))

int start_led_thread(int, int);

int source_fd = -1;

// Perform an IOCTL grab or release on an open keyboard handle
int ioctl_keyboard(int fd, int grab) {
  source_fd = fd;
  return ioctl(fd, EVIOCGRAB, grab);
}

// Acquire a filedescriptor as a uinput keyboard
int acquire_uinput_keysink(int fd, char *name, int vendor, int product, int version) {

  // Designate fd as a keyboard of all keys
  ioctl(fd, UI_SET_EVBIT, EV_KEY);
  int i;
  for (i=0; i < 256; i++) {
    ioctl(fd, UI_SET_KEYBIT, i);
  }

  // setup leds
  // 1. retrieve available leds from source
  u_int8_t supported_leds[LED_MAX / 8 + 1] = {0};
  if (source_fd >= 0)
    ioctl(source_fd, EVIOCGBIT(EV_LED, LED_MAX), supported_leds);
  else
    supported_leds[0] = 0b11110000;  // first 4 leds

  // 2. expose those leds
  ioctl(fd, UI_SET_EVBIT, EV_LED);
  for (i=0; i < LED_MAX; i++) {
    if (test_bit(i, supported_leds)) {
      ioctl(fd, UI_SET_LEDBIT, i);
    }
  }

  // Set the vendor details
  struct uinput_setup usetup;
  memset(&usetup, 0, sizeof(usetup));
  usetup.id.bustype = BUS_USB;
  usetup.id.vendor = vendor;
  usetup.id.product = product;
  usetup.id.version = version;

  strcpy(usetup.name, name);
  ioctl(fd, UI_DEV_SETUP, &usetup);

  ioctl(fd, UI_SET_EVBIT, EV_SYN);

  // Create the device
  ioctl(fd, UI_DEV_CREATE);
  start_led_thread(fd, source_fd);
  return 0;
}

// Release a uinput keyboard
int release_uinput_keysink(int fd) {
  return ioctl(fd, UI_DEV_DESTROY);
}

// Send a keyboard event through a file-descriptor
int send_event(int fd, int type, int code, int val, int s, int us) {
  struct input_event ie;
  ie.type = type;
  ie.code = code;
  ie.value = val;
  ie.time.tv_sec = s;
  ie.time.tv_usec = us;
  return write(fd, &ie, sizeof(ie));
}

// Print information about memory layout of input_event
void input_event_info() {
  struct input_event event;
  printf("sizeof  event is:               %d\n", (int) sizeof(event));
  printf("alignof event is:               %d\n", (int) __alignof__(event));
  printf("sizeof  event.time is:          %d\n", (int) sizeof(event.time));
  printf("alignof event.time is:          %d\n", (int) __alignof__(event.time));
  printf("sizeof  event.time.tv_sec is:   %d\n", (int) sizeof(event.time.tv_sec));
  printf("alignof event.time.tv_sec is:   %d\n", (int) __alignof__(event.time.tv_sec));
  printf("sizeof  event.time.tv_usec is:  %d\n", (int) sizeof(event.time.tv_usec));
  printf("alignof event.time.tv_usec is:  %d\n", (int) __alignof__(event.time.tv_usec));
  printf("sizeof  event.type is:          %d\n", (int) sizeof(event.type));
  printf("alignof event.type is:          %d\n", (int) __alignof__(event.type));
  printf("sizeof  event.code is:          %d\n", (int) sizeof(event.code));
  printf("alignof event.code is:          %d\n", (int) __alignof__(event.code));
  printf("sizeof  event.value is:         %d\n", (int) sizeof(event.value));
  printf("alignof event.value is:         %d\n", (int) __alignof__(event.value));
}

struct _thread_args {
  int source_fd;
  int sink_fd;
};

void* _led_sync_thread(void* arg_p) {
  struct _thread_args *args = (struct _thread_args*) arg_p;

  struct input_event event;
  int r;


  struct pollfd fds[1];
  fds[0].fd = args->sink_fd;
  fds[0].events = POLLIN;

  while (1) {
    r = poll(fds, 1, 5000);
    if (r > 0) {
      if (fds[0].revents) {
        r = read(fds[0].fd, &event, sizeof(event));
        if (r < 0)
          break;
        
        r = write(args->source_fd, &event, sizeof(event));
        if (r != sizeof(event)) {
          printf("error writing to source device\n");
          break;
        }
      } else {
        printf("error\n");
      }
    } else {
      printf("timeout\n");
    }
  } 

  printf("Closing thread (error %d)\n", errno);

  free(arg_p);
}

int start_led_thread(int sink_fd, int source_fd) {
  struct _thread_args *args = malloc(sizeof(struct _thread_args));
  args->sink_fd = sink_fd;
  args->source_fd = source_fd;

  pthread_t thread;
  int err = pthread_create(&thread, NULL, &_led_sync_thread, (void*)args);
  if (err != 0) 
    return err;
  pthread_detach(thread);
  return 0;
}