{-|
Module      : KMonad.Api.KeyIO.Linux.Types
Description : The types particular to Linux KeyIO
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Api.KeyIO.Linux.Types
  ( -- * The LinuxKeyEvent datatype, its constructors, and instances
    -- $types
    LinuxKeyEvent(..)
  , linuxKeyEvent
  , sync
  )
where

import Control.Lens
import Foreign.C.Types (CInt)
import Foreign.C.String (CString)

import KMonad.Core

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
sync t = LinuxKeyEvent (fi $ t^.s :: CInt, fi $ t^.ns :: CInt, n, n, n)
  where n = 0 :: CInt


-------------------------------------------------------------------------------
-- Casting between LinuxKeyEvents and core KeyEvents
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

-- | Translate KeyEvents to LinuxKeyEvents for writing
fromLinuxKeyEvent :: LinuxKeyEvent -> Maybe KeyEvent
fromLinuxKeyEvent (LinuxKeyEvent (s', ns', typ, c, val))
  | typ == 1  = Just $ mkKeyEvent et (toEnum . fromIntegral $ c) t
  | otherwise = Nothing
    -- Unsupported events:
    -- 0:  Sync events
    -- 4:  Scan events
    -- 17: FF_STATUS, occurs after engaging CapsLock for example
    -- Perhaps others
  where
    t   = mkTime s' ns'
    et  = case val of
            0 -> Disengaged
            1 -> Engaged
            v -> error $ "Unparseable value: " <> show v

-- | Translate LinuxKeyEvents to KeyEvents for reading
toLinuxKeyEvent :: KeyEvent -> LinuxKeyEvent
toLinuxKeyEvent e = LinuxKeyEvent (fi $ e^.time.s, fi $ e^.time.ns, 1, c, val)
  where
    c, val :: CInt
    (c, val) = ( fromIntegral . fromEnum $ e^.keyCode
               , case e^.switchState of Disengaged -> 0
                                        Engaged    -> 1)

instance AsKeyEvent LinuxKeyEvent where
  _KeyEvent = prism' toLinuxKeyEvent fromLinuxKeyEvent

