-- |

module K.Shell.KeyIO.Initial
  ( -- * Basic types
    KioEvent(..)

    -- * Env types and lenses
  , KeySnk(..)
  , KeySrc(..)
  , KeyRepeatEnv(..)
  , KioEnv(..)
  , HasKeySnk(..)
  , HasKeySrc(..)
  , HasKeyRepeatEnv(..)
  , HasKioEnv(..)

    -- * Linux code
  , LinEvent(..)
  , getLinEvent
  , _LinEvent
  , syncEvent

    -- * Reexports
  , module K.Shell.Initial
  , module K.Shell.Cfg
  , module K.Shell.Logging
  )
where

import K.Shell.Initial
import K.Shell.Cfg
import K.Shell.Logging

import Data.Binary
import Data.Binary.Get


-- basic types -----------------------------------------------------------------

data KioEvent
  = KioPress   Keycode
  | KioRelease Keycode
  | KioRepeat  Keycode
  deriving Show

newtype KeySnk = KeySnk { snkEvent :: KioEvent -> IO () }
newtype KeySrc = KeySrc { srcEvent :: IO KioEvent }

data KeyRepeatEnv = KeyRepeatEnv

data KioEnv = KioEnv
  { _eKeySnk :: KeySnk
  , _eKeySrc :: KeySrc
  , _eKeyRepeatEnv :: KeyRepeatEnv
  }

-- lenses ----------------------------------------------------------------------

makeClassy ''KeySnk
makeClassy ''KeySrc
makeClassy ''KeyRepeatEnv
makeClassy ''KioEnv

instance HasKeySnk KioEnv where keySnk = eKeySnk
instance HasKeySrc KioEnv where keySrc = eKeySrc
instance HasKeyRepeatEnv KioEnv where keyRepeatEnv = eKeyRepeatEnv

-- linux types -----------------------------------------------------------------

newtype LinEvent = LinEvent (Word64, Word64, Word16, Word16, Int32)
  deriving (Eq, Show)

syncEvent :: LinEvent
syncEvent = LinEvent (0, 0, 0, 0, 0)

getLinEvent :: Get LinEvent
getLinEvent = do
  s <- getWord64le -- Seconds
  n <- getWord64le -- Nanoseconds
  t <- getWord16le -- Event
  c <- getWord16le -- Keycode
  v <- getInt32le  -- Event type
  pure $ LinEvent (s, n, t, c, v)

-- | Conversion between 'LinEvent' tuples and 'KioEvent' values
--
-- NOTE: this breaks certain prism laws, since we discard all info about timing.
-- However, we never use timing, so this is irrelevant.
_LinEvent :: Prism' LinEvent KioEvent
_LinEvent = prism' embed extract where
  embed (KioRelease c) = LinEvent (0, 0, 1, fromIntegral c, 0)
  embed (KioPress c)   = LinEvent (0, 0, 1, fromIntegral c, 1)
  embed (KioRepeat c)  = LinEvent (0, 0, 1, fromIntegral c, 2)
  extract (LinEvent (_, _, 1, c, v)) = case v of
    0 -> Just . KioRelease $ fromIntegral c
    1 -> Just . KioPress   $ fromIntegral c
    2 -> Just . KioRepeat  $ fromIntegral c
    _ -> Nothing
  extract _ = Nothing
