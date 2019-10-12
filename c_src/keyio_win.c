// Necessary to get mingw to compile for some reason
#define WINVER 0x0601
#define _WIN32_WINNT 0x0601

#include <windows.h>
#include <stdbool.h>
#include <stdio.h>


// Whether an event is a press or release
enum EVENT_TYPE {KEY_PRESS, KEY_RELEASE, KEY_REPEAT};

// All the information KMonad needs about a key-event
typedef struct KEY_EVENT {
  EVENT_TYPE type;
  DWORD      keycode;
}

// Variables we need to access globally
HANDLE readPipe  = NULL;
HANDLE writePipe = NULL;
HHOOK hookHandle;

// Print last error and exit program
void last_error()
{ LPVOID lpMsgBuf;
  DWORD dw = GetLastError();

  FormatMessage(FORMAT_MESSAGE_ALLOCATE_BUFFER |
                FORMAT_MESSAGE_FROM_SYSTEM |
                FORMAT_MESSAGE_IGNORE_INSERTS,
                NULL,
                dw,
                MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
                (LPTSTR) &lpMsgBuf,
                0, NULL);

  printf("Error: %s", lpMsgBuf);
  exit(dw);
}

// Callback we insert into windows that writes events to pipe
LRESULT CALLBACK keyHandler(int nCode, WPARAM wParam, LPARAM lParam)
{
  // Cast the lParam to a keyboard-event struct
  KBDLLHOOKSTRUCT* e = (KBDLLHOOKSTRUCT*)(lParam);

  // Skip processing if nCode is 0 or this is an injected event
  if (nCode < 0 || e->flags & LLKHF_INJECTED) {
    return CallNextHookEx(hookHandle, nCode, wParam, lParam);
  };

  // Create the KEY_EVENT matching the current event
  EVENT_TYPE type;
  switch wParam {
      case WM_KEYDOWN:
        type = KEY_PRESS;
        break;

      case WM_SYSKEYDOWN:
        type = KEY_PRESS;
        break;

      case WM_KEYUP:
        type = KEY_RELEASE;
        break;

      case WM_SYSKEYUP:
        type = KEY_RELEASE;
        break;
    }

  KEY_EVENT ev;
  ev.type    = type;
  ev.keycode = e->vkCode;


  // Write the event to the pipe
  DWORD dwWritten;
  WriteFile(writePipe, &ev, sizeof(ev), &dwWritten, NULL);

  // Comment this out when ready for final
  return CallNextHookEx(hookHandle, nCode, wParam, lParam);
}

// When called, get the next event from the pipe
KEY_EVENT* wait_key()
{
  DWORD dwRead;
  KEY_EVENT* e;

  ReadFile(readPipe, e, sizeof(e), &dwRead, NULL);
  return e;
}

// Insert the keyboard hook and start the monitoring process
int grab_kb()
{
  // Insert the hook, error on failure
  hookHandle = SetWindowsHookEx(WH_KEYBOARD_LL, keyHandler, NULL, 0);
  if (hookHandle == NULL) last_error();

  // Create the pipe, error on failure
  if ( !CreatePipe(&readPipe, &writePipe, NULL, 0) ) last_error();

  // Start an event-loop to keep the process alive
  MSG msg;
  while (GetMessage(&msg, NULL, 0, 0))
    { TranslateMessage(&msg);
      DispatchMessage(&msg); }

  // Cleanup and close
  UnhookWindowsHookEx(hookHandle);
  return(0);
}

// Return ms since system-start
DWORD time_since_start() { GetTickCount(); }

// Send key to the OS
void sendKey(KEY_EVENT e)
{
  INPUT ip;

  // Standard stuff we don't use
  ip.type           = INPUT_KEYBOARD;
  ip.ki.wScan       = 0;
  ip.ki.time        = 0;
  ip.ki.dwExtraInfo = 0;

  // Insert the KeyUp or KeyDown flag
  switch e.type {
      case KEY_PRESS:
        ip.ki.dwFlags = 0;
        break

      case KEY_RELEASE:
        ip.ki.dwFlags = KEYEVENTF_KEYUP;
        break
    }

  // Insert the raw keycode
  ip.ki.wVk = e.keycode;

  // Emit the event to the OS
  SendInput(1, &ip, sizeof(INPUT));
}





// OLD STUFF FOR FUTURE REFERENCE

// Tell windows to start sending all keyboard events to hTarget
/* bool HID_RegisterDevice(HWND hTarget, USHORT usage)
{
  RAWINPUTDEVICE hid[1];

  hid[0].usUsagePage = 0x01;
  hid[0].usUsage     = usage;
  hid[0].dwFlags     = RIDEV_INPUTSINK; // RIDEV_DEVNOTIFY RIDEV_NOLEGACY
  hid[0].hwndTarget  = hTarget;

  return RegisterRawInputDevices(&hid, 1, sizeof(RAWINPUTDEVICE));
}
 */
 /* void grab_keyboard(HWND hTarget)
{
    RAWINPUTDEVICE ri[1];

    ri[0].usUsagePage = 0x01;
    ri[0].usUsage     = 0x06;
    ri[0].dwFlags     = RIDEV_NOLEGACY | RIDEV_INPUTSINK;
    ri[0].hwndTarget  = hTarget;

    if (RegisterRawInputDevices(ri, 1, sizeof(ri[0])) == FALSE) {
        last_error();
    }
}
 */

 
/* 
// Tell windows to stop sending all keyboard events to hTarget
void HID_UnregisterDevice(USHORT usage)
{
  RAWINPUTDEVICE hid;

  hid.usUsagePage = 1;
  hid.usUsage     = usage;
  hid.dwFlags     = RIDEV_REMOVE;
  hid.hwndTarget  = NULL;

  RegisterRawInputDevices(&hid, 1, sizeof(RAWINPUTDEVICE));
} */
 
 
 /* Make windows class and window
    //const wchar_t CLASS_NAME[] = L"Sample Window Class";
    static const char* CLASS_NAME = "MESSAGE_ONLY";

    WNDCLASSEX wc;
    wc.cbSize        = sizeof(WNDCLASSEX);
    wc.style         = 0x0;
    wc.lpszClassName = CLASS_NAME;
    wc.lpfnWndProc   = callback;
    wc.hInstance     = hInstance;

    wc.hIcon         = LoadIcon(NULL, IDI_APPLICATION);
    wc.hIconSm       = LoadIcon(NULL, IDI_APPLICATION);
    wc.hCursor       = LoadCursor(NULL, IDC_ARROW);
    wc.lpszMenuName  = 0;
    wc.cbClsExtra    = 0;
    wc.cbWndExtra    = 0;
    wc.hbrBackground = NULL;

    fprintf(stdout, "trying to make window class\n");
    if ( !RegisterClassEx(&wc) ) { last_error(); }
    fprintf(stdout, "succeeded");

    fprintf(stdout, "hello\n");
    HWND hwnd = CreateWindowEx(
        0,
        CLASS_NAME, // Window class
        "kmonad",  // Window text
        WS_OVERLAPPEDWINDOW, // Window style

        // Size and position
        0,0,0,0,
        HWND_MESSAGE, // Parent: message only window
        NULL, // Menu
        hInstance, // Instance
        NULL // Additional data
    );
    ShowWindow(hwnd, cmd_show);
 
 
// Windows event callback
LRESULT CALLBACK callback(HWND hwnd, UINT uMsg, WPARAM wParam, LPARAM lParam)
{
    switch (uMsg) {

        case WM_DESTROY: {
            PostQuitMessage(0);
            return 0;
        }

        case WM_CLOSE: {
            DestroyWindow(hwnd);
            return 0;
        }

        case WM_INPUT: {
            fprintf(stdout, "hello");
            UINT dwSize;

            GetRawInputData((HRAWINPUT)lParam, RID_INPUT, NULL, &dwSize, sizeof(RAWINPUTHEADER));
            //LPBYTE lpb = new BYTE[dwSize];
            //inf (lpb == NULL) {
            //  return 0; }
            return 0;

        }
    }
    return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrev, LPSTR cmd_line, int cmd_show)
{

 */
