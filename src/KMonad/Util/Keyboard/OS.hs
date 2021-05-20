{-# LANGUAGE CPP #-}
module KMonad.Util.Keyboard.OS
  ( Keycode, keycodeNames )
where

{-
NOTE: This 'portal' only exports the types *required* by 'KMonad.Util.Keyboard'.
The OS-specific modules may export other stuff, to be imported in OS-specific IO
modules, like Linux' RawEvent specifications.

NOTE: This means that any OS-specific module must export *at least* a Keycode
type and a 'M.HashMap CoreName Keycode' named @keycodeNames@.
-}

#ifdef linux_HOST_OS
import KMonad.Util.Keyboard.Linux
#endif

#ifdef mingw32_HOST_OS
import KMonad.Util.Keyboard.Windows
#endif

#ifdef darwin_HOST_OS
import KMonad.Util.Keyboard.Mac
#endif
