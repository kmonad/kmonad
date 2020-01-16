{-|
Module      : KMonad.Keyboard.IO.Linux.Types
Description : The types particular to Linux key IO
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Keyboard.IO.Linux.Types
  ( -- * The LinuxKeyEvent datatype, its constructors, and instances
    -- $types
    LinuxKeyEvent(..)
  , linuxKeyEvent
  , sync

    -- * Casting between 'KeyEvent' and 'LinuxKeyEvent'
    -- $linuxev
  , toLinuxKeyEvent
  , fromLinuxKeyEvent

    -- * Reexport common modules
  , module KMonad.Keyboard
  , module KMonad.Keyboard.IO
  )
where

import KPrelude

import Foreign.C.Types (CInt)
import RIO.Partial (toEnum)

import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Util


--------------------------------------------------------------------------------
-- $helper

fi :: Integral a => a -> CInt
fi = fromIntegral

--------------------------------------------------------------------------------
-- $types
--
-- Linux produces a stream of binary data representing all its input events
-- through the \/dev\/input files. Each event is represented by 5 numbers:
-- seconds, microseconds, event-type, event-code, and event-value. For more
-- explanation look at: https://www.kernel.org/doc/Documentation/input/input.txt

-- | The LinuxKeyEvent datatype
newtype LinuxKeyEvent = LinuxKeyEvent (CInt, CInt, CInt, CInt, CInt)
  deriving Show

instance Display LinuxKeyEvent where
  textDisplay (LinuxKeyEvent (s, ns, typ, c, val)) = mconcat
         [ textDisplay $ mkTime s ns , ": "
         , "type: ", tshow typ, ",  "
         , "code: ", tshow c,   ",  "
         , "val: ",  tshow val
         ]

-- | A smart constructor that casts from any integral
linuxKeyEvent
  :: (Integral a, Integral b, Integral c, Integral d, Integral e)
  => (a, b, c, d, e) -- ^ The tuple representing the event
  -> LinuxKeyEvent   -- ^ The LinuxKeyEvent
linuxKeyEvent (a, b, c, d, e) = LinuxKeyEvent (f a, f b, f c, f d, f e)
  where
    f :: Integral a => a -> CInt
    f = fromIntegral

-- | Constructor for linux sync events. Whenever you write an event to linux,
-- you need to emit a 'sync' to signal to linux that it should sync all queued
-- updates.
sync :: Time -> LinuxKeyEvent
sync t = LinuxKeyEvent (fi $ t^._s, fi $ t^._ns, 0, 0, 0)


-------------------------------------------------------------------------------
-- $linuxev
--
-- We only represent a subset of all the possible input events produced by
-- Linux. First of all, we disregard all event types that are not key events, so
-- we quietly ignore all sync and scan events. There other other events that are
-- there to do things like toggle LEDs on your keyboard that we also ignore.
--
-- Furthermore, within the category of KeyEvents, we only register presses and
-- releases, and completely ignore repeat events.
--
-- The correspondence between LinuxKeyEvents and core KeyEvents can best be read
-- in the above-mentioned documentation, but the quick version is this:
--   Typ:  1 = KeyEvent            (see below)
--         4 = 'scancode' event    (we neither read nor write)
--         0 = 'sync' event        (we don't read, but do generate for writing)
--   Val:  for keys: 0 = Release, 1 = Press, 2 = Repeat
--         for sync: always 0
--   Code: for keys: an Int value corresponding to a keycode
--           see: https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h
--         for sync: always 0

-- | Translate a 'LinuxKeyEvent' to a kmonad 'KeyEvent'
fromLinuxKeyEvent :: LinuxKeyEvent -> Maybe KeyEvent
fromLinuxKeyEvent (LinuxKeyEvent (s, ns, typ, c, val))
  | typ == 1 && val == 0 = Just . atTime t $ keyRelease kc
  | typ == 1 && val == 1 = Just . atTime t $ keyPress   kc
  | otherwise = Nothing
  where
    t  = mkTime s ns
    kc = toEnum . fromIntegral $ c -- This is theoretically partial, but practically not

-- | Translate kmonad 'KeyEvent's to 'LinuxKeyEvent's for writing
toLinuxKeyEvent :: KeyEvent -> LinuxKeyEvent
toLinuxKeyEvent e = LinuxKeyEvent (fi $ e^._s, fi $ e^._ns, 1, c, val)
  where
    c   = fi . fromEnum $ e^.keycode
    val = if isPress e then 1 else 0
