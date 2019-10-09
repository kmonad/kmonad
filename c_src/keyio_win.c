#include <windows.h>

const USHORT HID_KEYBOARD = 6;

// Tell windows to start sending all keyboard events to hTarget
bool HID_RegisterDevice(HWND hTarget, USHORT usage)
{
  RAWINPUTDEVICE hid;

  hid.usUsagePage = 1;
  hid.usUsage     = usage;
  hid.dwFlags     = RIDEV_INPUTSINK; // RIDEV_DEVNOTIFY RIDEV_NOLEGACY
  hid.hwndTarget  = hTarget;

  return !!RegisterRawInputDevices(&hid, 1, sizeof(RAWINPUTDEVICE));
}

// Tell windows to stop sending all keyboard events to hTarget
void HID_UnregisterDevice(USHORT usage)
{
  RAWINPUTDEVICE hid;

  hid.usUsagePage = 1;
  hid.usUsage     = usage;
  hid.dwFlags     = RIDEV_REMOVE;
  hid.hwndTarget  = NULL;

  RegisterRawInputDevices(&hid, 1, sizeof(RAWINPUTDEVICE));
}

// WinMain is the entry-point for windows applications when they run
// Use the /Subsystem linker to hide window?
int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE, LPSTR cmd_line, int cmd_show)
{
  WNDCLASS wc;
  RegisterClass(&wc);

  HWND hwnd = CreateWindow(
                           0, // Optional styles
                           CLASS_NAME, // Window class
                           "test",  // Window text
                           WC_OVERLAPPEDWINDOW, // Window style

                           // Size and position
                           CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, CW_USEDEFAULT, 

                           NULL, // Parent
                           NULL, // Menu
                           hInstance, // Instance
                           NULL, // Additional data

                         );

  // ShowWindow(hwnd, nCmdShow); Can keep invisible?
  // Register keyboard
  HID_RegisterDevice(hwnd, HID_KEYBOARD);

  MSG msg;
  while (GetMessage(&msg, NULL, 0, 0))
  {
    // Listen to keyboard
  }
  

  // Unregister keyboard
  HID_UnregisterDevice(hwnd, HID_KEYBOARD);
}


// So: to capture input, we need to launch the keyboard listener. This needs to
// create an (invisible) window that windows will send RAWINPUT to, that we can
// then get into KMonad using the FFI.

// Subsystem linker?

// Synthesize keystrokes 
// use: SendInput
