#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <string.h>
#include <unistd.h>
#include <linux/input.h>
#include <linux/uinput.h>
#include <fcntl.h>

// Perform an IOCTL grab or release on an open keyboard handle
int ioctl_keyboard(int fd, int grab) {
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

  // Set the vendor details
  struct uinput_setup usetup;
  memset(&usetup, 0, sizeof(usetup));
  usetup.id.bustype = BUS_USB;
  usetup.id.vendor = vendor;
  usetup.id.product = product;
  usetup.id.version = version;

  strcpy(usetup.name, name);
  ioctl(fd, UI_DEV_SETUP, &usetup);

  // Create the device
  ioctl(fd, UI_DEV_CREATE);

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

