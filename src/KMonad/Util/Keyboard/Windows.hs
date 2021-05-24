module KMonad.Util.Keyboard.Windows

where

import KMonad.Prelude

import Foreign.Storable
import KMonad.Util.Keyboard.Common

--------------------------------------------------------------------------------
-- $keycode

-- | In Windows we use 'Word32', the windows-native keycode type
newtype Keycode = Keycode { unKeycode :: Word32 }
  deriving (Eq, Ord, Num, Show, Enum, Hashable)

--------------------------------------------------------------------------------
-- $evtyp

-- | Windows uses 2 types of events, presses and releases.
--
-- In windows, repeat events are signalled by repeated press events. The type is
-- ordered in a way to line up with the integers Windows uses to encode for
-- there events. I.e. 0=Press, 1=Release
data EvType = WindowsPress | WindowsRelease
  deriving (Eq, Show, Enum)

-- | Prism between Linux and Haskell representation of event-types
_EvType :: Prism' Word8 EvType
_EvType = prism' (fi . fromEnum) $ \i ->
  ([WindowsPress, WindowsRelease] ^? ix (fi i))
 
--------------------------------------------------------------------------------
-- $raw

-- | The RawEvent datatype
--
-- It contains a 'Word8' signifying whether the event was a Press (0) or Release
-- (1), and a 'Word32' (Windows @DWORD@) signifying the keycode value.
data RawEvent = RawEvent
  { _reVal  :: Word8  -- ^ press or release
  , _reCode :: Word32 -- ^ keycode
  } deriving (Eq, Ord, Show)
makeClassy ''RawEvent

-- | This lets us send 'WinKeyEvent's between Haskell and C.
instance Storable RawEvent where
  alignment _ = 4 -- lowest common denominator of: 1 4
  sizeOf    _ = 8 -- (1 + 3-padding) + 4
  peek ptr = do
    s <- peekByteOff ptr 0
    c <- peekByteOff ptr 4
    return $ RawEvent s c
  poke ptr (RawEvent s c) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 c

-- | Make a new raw event
mkRaw :: EvType -> Keycode -> RawEvent
mkRaw p c = RawEvent
  { _reCode = unKeycode c
  , _reVal  = p ^. re _EvType }
