module KMonad.Core.SpecialSymbol

where

import Control.Lens
import Data.Text (Text)

import KMonad.Core.Keyboard
import KMonad.Core.KeyCode

data SpecialSymbol = SpecialSymbol
  { _name       :: Text
  , _chr        :: Char
  , _composeSeq :: Maybe KeySequence
  } deriving (Eq, Show)
makeClassy ''SpecialSymbol




-- specialSymbols :: [SpecialSymbol]
-- specialSymbols = let ss (a, b, c) = SpecialSymbol a b c in map ss
--   [ -- Currency
--     ("Euro sign"    , '€', Just $ tap KeyEqual <> tap KeyC) -- [KeyEqual, KeyC])
  -- , ("Pount sign"   , '₤', Just [KeyEqual, KeyL])

--     -- Fractions
--   , ("One-half"     , '½', Just [Key1, Key2])
--   , ("Two-thirds"   , '⅔', Just [Key2, Key3])



  -- ]

-- --------------------------------------------------------------------------------
-- -- $unicode

-- -- newtype Unicode = Unicode Int
-- --   deriving (Eq, Show, Bounded, Enum, Ord)

-- -- fromChar :: Char -> Unicode
-- -- fromChar = Unicode . fromEnum

-- -- showSeq :: Unicode -> T.Text
-- -- showSeq (Unicode c) = T.pack (showIntAtBase 16 intToDigit c $ "")

-- -- test :: Char
-- -- test = 'Ñ'

-- -- test2 :: Char
-- -- test2 = 'ñ'
