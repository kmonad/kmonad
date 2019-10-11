// Necessary to get mingw to compile for some reason
#define WINVER 0x0601
#define _WIN32_WINNT 0x0601

#include <windows.h>
#include <stdbool.h>
#include <stdio.h>
#include <winsock2.h>

HHOOK hookHandle;
LRESULT CALLBACK keyHandler(int nCode, WPARAM wParam, LPARAM lParam);
void last_error ();

WSADATA wsaData;

int main()
{
	
	hookHandle = SetWindowsHookEx(WH_KEYBOARD_LL, keyHandler, NULL, 0);
	if (hookHandle == NULL) last_error();
	
	fprintf(stdout, "starting loop!");
	
	MSG msg;
	while (GetMessage(&msg, NULL, 0, 0))
	{
		fprintf(stdout, "Message!\n");
		TranslateMessage(&msg);
		DispatchMessage(&msg);
	} 
	fprintf(stdout, "finished loop!");
	
	UnhookWindowsHookEx(hookHandle);
  
	return (0);

}


// So: to capture input, we need to launch the keyboard listener. This needs to
// create an (invisible) window that windows will send RAWINPUT to, that we can
// then get into KMonad using the FFI.

// Subsystem linker?

// Synthesize keystrokes 
// use: SendInput


void last_error()
{
	LPVOID lpMsgBuf;
	//LPVOID lpDisplayBuf;
	DWORD dw = GetLastError();
	
	FormatMessage(
		FORMAT_MESSAGE_ALLOCATE_BUFFER |
		FORMAT_MESSAGE_FROM_SYSTEM |
		FORMAT_MESSAGE_IGNORE_INSERTS,
		NULL,
		dw,
		MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT),
		(LPTSTR) &lpMsgBuf,
		0, NULL);
	
	fprintf(stdout, lpMsgBuf);
	exit(dw);
	//if (dw == ERROR_IS_SUBSTED) { fprintf(stdout, "boop"); };
	//fprintf(stdout, "\n%lu", dw);
}

LRESULT CALLBACK keyHandler(int nCode, WPARAM wParam, LPARAM lParam) {
	fprintf(stdout, "keyping\n");
	if (nCode < 0) {
		return CallNextHookEx(hookHandle, nCode, wParam, lParam);
	};
	
	// Put handling here in future
	return CallNextHookEx(hookHandle, nCode, wParam, lParam);
}

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
			//	return 0; }
			return 0;
			
		}	
	}
	return DefWindowProc(hwnd, uMsg, wParam, lParam);
}

int WINAPI WinMain(HINSTANCE hInstance, HINSTANCE hPrev, LPSTR cmd_line, int cmd_show)
{

 */