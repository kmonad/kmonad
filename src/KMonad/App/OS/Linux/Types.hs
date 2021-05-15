module KMonad.App.OS.Linux.Types
  ( Keycode(..)
  , GetKey, PutKey
  , RawEvent(..), HasRawEvent(..), mkRaw, mkSync
  , keyNames, keyAliases
  , EvType(..), _EvType
  , module KMonad.App.KeyIO.Common.Types
  )

where


import KMonad.Prelude
import KMonad.Util.Time
import KMonad.App.OS.Common

import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $keycode

-- | In Linux we use 'Word16', the linux-native keycode type
--
-- NOTE:
-- * Required by KMonad.App: Eq, Show, Hashable
-- * Used to keep a set in Evdev: Ord
-- * Used to specify keycodes as numbers and ranges: Num, Enum
newtype Keycode = Keycode { unKeycode :: Word16 }
  deriving (Eq, Ord, Num, Show, Enum, Hashable)

--------------------------------------------------------------------------------
-- $raw

-- | The RawEvent datatype
--
-- Linux produces a stream of binary data representing all its input events
-- through the \/dev\/input files. Each event is represented by 5 numbers:
-- seconds, microseconds, event-type, event-code, and event-value. For more
-- explanation look at: https://www.kernel.org/doc/Documentation/input/input.txt
--
-- We parse the entire event, but discard everything except the keycode.
data RawEvent = RawEvent
  { _leS    :: !Word64  -- ^ The seconds component of system time
  , _leNS   :: !Word64  -- ^ The nanoseconds component of system time
  , _leType :: !Word16  -- ^ The type signals the kind of event (we only use EV_KEY)
  , _leCode :: !Word16  -- ^ The keycode indentifier of the key
  , _leVal  :: !Int32   -- ^ Whether a press, release, or repeat event
  } deriving (Show)
makeClassy ''RawEvent

-- | Generate a new raw event
mkRaw :: IO m => Switch -> Keycode -> m RawEvent
mkRaw p c = do
  t <- getCurrentSystemTime
  pure $ RawEvent
    { _leS    = fi $ t^._s
    , _leNS   = fi $ t^._ns
    , _leType = 1
    , _leCode = unKeycode c
    , _leVal  = if (p == Press) then 1 else 0
    }

-- | Generate a new sync event
mkSync :: IO m => m RawEvent
mkSync = do
  t <- getCurrentSystemTime
  pure $ RawEvent
    { _leS    = fi $ t^._s
    , _leNS   = fi $ t^._ns
    , _leType = 0
    , _leCode = 0
    , _leVal  = 0
    }

--------------------------------------------------------------------------------

data EvType = Release | Press | Repeat deriving (Eq, Ord, Enum, Show)

_EvType :: Prism' Int32 EvType
_EvType = prism' (fi . fromEnum) $ \i -> ([Release, Press, Repeat] ^? ix (fi i))

