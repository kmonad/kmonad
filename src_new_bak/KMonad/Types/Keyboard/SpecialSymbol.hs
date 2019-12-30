{-|
Module      : KMonad.Types.Keyboard.SpecialSymbol
Description : Sequences mapped to special symbols
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)
-}
module KMonad.Types.Keyboard.SpecialSymbol
  ( -- * SpecialSymbol datatype and lenses
    SpecialSymbol
  , mkSpecialSymbol
  , utfSeq

    -- * DeadKey datatype
  , DeadKey(..)

    -- * Classy lenses shared by both types
  , HasComposeSeq(..)
  )
where

import Prelude

import Data.Char (intToDigit)
import Data.Maybe (fromJust)
import Numeric

import KMonad.Types.Name
import KMonad.Types.Keyboard.Keycode
import KMonad.Types.Keyboard.KeySequence


--------------------------------------------------------------------------------

-- | The SpecialSymbol datatype
data SpecialSymbol = SpecialSymbol
  { _ssName       :: !Name
  , _chr          :: !Char
  , _ssComposeSeq :: !(Maybe KeySequence)
  , _utfSeq       :: !KeySequence
  } deriving (Eq, Show)
makeClassy ''SpecialSymbol

-- | Comparison based on the 'Ord' of its 'chr'
instance Ord SpecialSymbol where
  a `compare` b = (a^.chr) `compare` (b^.chr)

-- | Hook up the Name lens
instance HasName SpecialSymbol       where name = ssName

-- | A smart constructor for 'SpecialSymbol's
mkSpecialSymbol :: Name -> Char -> Maybe KeySequence -> SpecialSymbol
mkSpecialSymbol n c cmp = SpecialSymbol
  { _ssName       = n
  , _chr          = c
  , _ssComposeSeq = cmp
  , _utfSeq       = utfSequence c
  }

-- | Return the hex-string representation of a character
utfString :: Char -> String
utfString c = showIntAtBase 16 intToDigit (fromEnum c) $ ""

-- | Return the 'KeySequence' required to perform the UTF-code entry for a char
utfSequence :: Char -> KeySequence
utfSequence = concatMap (mkKeyTap . fromJust . kcFromChar) . utfString
  -- fromJust is justified here because 'utfString' is always a hex-string, and
  -- we know that we have full correspondence between letters/numbers and
  -- keycodes


--------------------------------------------------------------------------------

-- | The 'DeadKey' sequence that signals to an OS that a 'SpecialSymbol'
-- sequence is coming
data DeadKey = DeadKey
  { _dkComposeSeq :: !(Maybe KeySequence)
  } deriving (Eq, Ord, Show)
makeClassy ''DeadKey

--------------------------------------------------------------------------------

-- | A class that describes 'having a Compose sequence'
class HasComposeSeq a where
  composeSeq :: Lens' a (Maybe KeySequence)

instance HasComposeSeq SpecialSymbol where composeSeq = ssComposeSeq
instance HasComposeSeq DeadKey       where composeSeq = dkComposeSeq
