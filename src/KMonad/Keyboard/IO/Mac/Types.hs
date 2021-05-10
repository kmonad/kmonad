module KMonad.Keyboard.IO.Mac.Types
  ( MacError(..)
  , MacKeyEvent
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
  | BadMacSwitch     MacSwitch  -- ^ Error interpreting 'MacSwitch'

instance Exception MacError
instance Show MacError where
  show e = case e of
    NoMacKeycodeTo   c -> "Cannot translate to mac keycode: "   <> show c
    NoMacKeycodeFrom i -> "Cannot translate from mac keycode: " <> show i
    BadMacSwitch     s -> "Cannot interpret mac switch: "       <> show s
instance Exception [MacError]

--------------------------------------------------------------------------------
-- $typ

type MacSwitch  = Word64           -- ^ Type alias for the switch value
type MacKeycode = (Word32, Word32) -- ^ Type alias for the Mac keycode

-- | 'MacKeyEvent' is the C-representation of a a 'KeyEvent' for our Mac API.
--
-- It contains a 'Word8' signifying whether the event was a Press (0)
-- or Release (1), and a 'Word32' (uint32_t) signifying the Mac
-- keycode (the upper 16 bits represent the IOKit usage page, and the
-- lower 16 bits represent the IOKit usage).
--
-- NOTE: Mac and Linux keycodes do not line up. Internally we use Linux
-- Keycodes for everything, we translate at the KeyIO stage (here).
newtype MacKeyEvent = MacKeyEvent (MacSwitch, MacKeycode)
  deriving (Eq, Ord, Show)

-- | This lets us send 'MacKeyEvent's between Haskell and C.
instance Storable MacKeyEvent where
  alignment _ = 4
  sizeOf    _ = 16
  peek ptr = do
    s <- peekByteOff ptr 0
    p <- peekByteOff ptr 8
    u <- peekByteOff ptr 12
    return $ MacKeyEvent (s, (p, u))
  poke ptr (MacKeyEvent (s, (p, u))) = do
    pokeByteOff ptr 0 s
    pokeByteOff ptr 8 p
    pokeByteOff ptr 12 u

--------------------------------------------------------------------------------
-- $conv

fromMacSwitch :: MacSwitch -> Maybe Switch
fromMacSwitch s = case s of
  1 -> Just Press
  0 -> Just Release
  _ -> Nothing

toMacSwitch :: Switch -> MacSwitch
toMacSwitch s = if s == Press then 1 else 0

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
  Just c  -> Right $ MacKeyEvent (toMacSwitch (e^.switch), c)
  Nothing -> Left . NoMacKeycodeTo $ e^.keycode

-- | Convert a 'MacKeyEvent' to a 'KeyEvent'
--
-- NOTE: Same limitations as 'toMacKeyEvent' apply
fromMacKeyEvent :: MacKeyEvent -> Maybe (Either [MacError] KeyEvent)
fromMacKeyEvent (MacKeyEvent (s, (p, u)))
  | p == 7 && u <= 0x3    = Nothing
  | p == 7 && u >= 0xFFFF = Nothing
  | otherwise             = case (fromMacKeycode (p, u), fromMacSwitch s) of
      (Just c', Just s') -> Just (Right $ mkKeyEvent s' c')
      (Just _, Nothing)  -> Just (Left [BadMacSwitch s])
      (Nothing, Just _)  -> Just (Left [NoMacKeycodeFrom (p,u)])
      (Nothing, Nothing) -> Just (Left [BadMacSwitch s, NoMacKeycodeFrom (p,u)])

--------------------------------------------------------------------------------
-- $kc

-- | NOTE: This used to be a thing. It broke on merging master and develop. Will
-- be replaced with new keycode system.

kcMap :: M.HashMap MacKeycode Keycode
kcMap = undefined
