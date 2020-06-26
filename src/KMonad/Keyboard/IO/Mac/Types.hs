module KMonad.Keyboard.IO.Mac.Types
  ( MacError(..)
  , MacKeyEvent
  , mkMacKeyEvent
  , toMacKeyEvent
  , fromMacKeyEvent
  )

where

import KMonad.Prelude

import Foreign.Storable
import KMonad.Keyboard

import qualified RIO.HashMap as M


----------------------------------------------------------------------------
-- $err

-- | Everything that can go wrong with Mac Key-IO
data MacError
  = NoMacKeycodeTo   Keycode    -- ^ Error translating to 'MacKeycode'
  | NoMacKeycodeFrom MacKeycode -- ^ Error translating from 'MacKeycode'

instance Exception MacError
instance Show MacError where
  show e = case e of
    NoMacKeycodeTo   c -> "Cannot translate to mac keycode: "   <> show c
    NoMacKeycodeFrom i -> "Cannot translate from mac keycode: " <> show i

--------------------------------------------------------------------------------
-- $typ

type MacSwitch  = Word8  -- ^ Type alias for the switch value
type MacKeycode = Word32 -- ^ Type alias for the windows encoded keycode

-- | 'MacKeyEvent' is the C-representation of a a 'KeyEvent' for our Mac API.
--
-- It contains a 'Word8' signifying whether the event was a Press (0) or Release
-- (1), and a 'Word32' (uint32_t) signifying the *Mac keycode*.
--
-- NOTE: Mac and Linux keycodes do not line up. Internally we use Linux
-- Keycodes for everything, we translate at the KeyIO stage (here).
newtype MacKeyEvent = MacKeyEvent (MacSwitch, MacKeycode)
  deriving (Eq, Ord, Show)

-- | This lets us send 'MacKeyEvent's between Haskell and C.
instance Storable MacKeyEvent where
  alignment _ = 4 -- lowest common denominator of: 1 4
  sizeOf    _ = 8 -- (1 + 3-padding) + 4
  peek ptr = do
    s <- peekByteOff ptr 0
    c <- peekByteOff ptr 4
    return $ MacKeyEvent (s, c)
  poke ptr (MacKeyEvent (s, c)) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 4 c

mkMacKeyEvent :: MacSwitch -> MacKeycode -> MacKeyEvent
mkMacKeyEvent s e = MacKeyEvent (s, e)

--------------------------------------------------------------------------------
-- $conv

-- | Convert between 'MacSwitch' and 'Switch' representations.
--
-- NOTE: Although 'MacSwitch' could theoretically be something besides 0 or 1,
-- practically it can't, because those are the only values the API generates,
-- guaranteed.
_MacSwitch :: Iso' MacSwitch Switch
_MacSwitch = iso to' from'
  where
    to' w   = if w == 0 then Press else Release
    from' s = if s == Press then 0 else 1

-- | Lookup the corresponding 'Keycode' for this 'MacKeycode'
fromMacKeycode :: MacKeycode -> Maybe Keycode
fromMacKeycode = flip M.lookup kcMap

-- | Lookup the correspondig 'MacKeycode' for this 'Keycode'
toMacKeycode :: Keycode -> Maybe MacKeycode
toMacKeycode = flip M.lookup revMap
  where revMap = M.fromList $ (M.toList kcMap) ^.. folded . swapped

-- | Convert a 'KeyEvent' to a 'MacKeyEvent'
--
-- NOTE: Mac keycodes are different, and I am not confident I have full
-- coverage, therefore this conversion is not total. We are going to leave this
-- error-handling in until we are sure this is covered well. Once it lines up
-- perfectly, this is essentially an Iso.
toMacKeyEvent :: KeyEvent -> Either MacError MacKeyEvent
toMacKeyEvent e = case toMacKeycode $ e^.keycode of
  Just c  -> Right $ MacKeyEvent (e^.switch.from _MacSwitch, c)
  Nothing -> Left . NoMacKeycodeTo $ e^.keycode

-- | Convert a 'MacKeyEvent' to a 'KeyEvent'
--
-- NOTE: Same limitations as 'toMacKeyEvent' apply
fromMacKeyEvent :: MacKeyEvent -> Either MacError KeyEvent
fromMacKeyEvent (MacKeyEvent (s, c)) = case fromMacKeycode c of
  Just c' -> Right $ mkKeyEvent (s^._MacSwitch) c'
  Nothing -> Left . NoMacKeycodeFrom $ c


--------------------------------------------------------------------------------
-- $kc

-- | Mac does not use the same keycodes as Linux, so we need to translate.
--
-- FIXME: There are loads of missing correspondences, mostly for rare-keys. How
-- do these line up? Ideally this mapping would be total.
kcMap :: M.HashMap MacKeycode Keycode
kcMap = M.fromList $
  [ (0x04, KeyA)
  , (0x07, KeyD)
  , (0x09, KeyF)
  , (0x16, KeyS)
  , (0x2C, KeySpace)
  , (0xE0, KeyLeftCtrl)
  , (0xE1, KeyLeftShift)
  , (0xE2, KeyLeftAlt)
  , (0xE3, KeyLeftMeta)
  ]

