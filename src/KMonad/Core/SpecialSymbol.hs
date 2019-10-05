module KMonad.Core.SpecialSymbol

where

import Control.Lens
import Data.Char (intToDigit)
import Data.Maybe (fromJust)
import Data.Text (Text)
import Numeric

import KMonad.Core.Keyboard
import KMonad.Core.KeyCode
import KMonad.Core.Types

import qualified Data.Text as T

class HasChr a where
  chr :: Lens' a Char

--------------------------------------------------------------------------------

data SpecialSymbol = SpecialSymbol
  { _ssName     :: !Name
  , _ssChr      :: !Char
  , _composeSeq :: !(Maybe KeySequence)
  , _utfSeq     :: !KeySequence
  } deriving (Eq, Show)
makeClassy ''SpecialSymbol



instance Ord SpecialSymbol where
  a `compare` b = (a^.chr) `compare` (b^.chr)

instance HasName SpecialSymbol where name = ssName
instance HasChr  SpecialSymbol where chr  = ssChr

mkSpecialSymbol :: Name -> Char -> Maybe KeySequence -> SpecialSymbol
mkSpecialSymbol n c cmp = SpecialSymbol
  { _ssName     = n
  , _ssChr      = c
  , _composeSeq = cmp
  , _utfSeq     = utfSequence c
  }

-- | Return the hex-string representation of a character
utfString :: Char -> String
utfString c = showIntAtBase 16 intToDigit (fromEnum c) $ ""

-- | Return the 'KeySequence' required to perform the UTF-code entry for a char
utfSequence :: Char -> KeySequence
utfSequence = concatMap (tap . fromJust . kcFromChar) . utfString
  -- fromJust is justified here because 'utfString' is always a hex-string, and
  -- we know that we have full correspondence between letters/numbers and
  -- keycodes


--------------------------------------------------------------------------------

data DeadKey = DeadKey
  { _dkKeyCode :: !KeyCode
  } deriving (Eq, Show)
makeClassy ''DeadKey

instance HasKeyCode DeadKey where
  keyCode = dkKeyCode
