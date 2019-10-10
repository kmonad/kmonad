#include <windows.h>
#include <stdio.h>


hook = SetWindowsHookEx(WH_KEYBOARD_LL, callback, hwnd, 0);

LRESULT callback(int nCode, WPARAM wParam, LPARAM lParam) {

};

//HHOOK kbHook = SetWindowsHookEx(WH_KEYBOARD_LL, &callback, GetModuleHandle(NULL), NULL);
