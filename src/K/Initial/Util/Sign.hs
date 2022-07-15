-- |

module K.Initial.Util.Sign where

import K.Initial

-- basic sign type -------------------------------------------------------------

newtype Sign = Sign { _sign :: Text }

instance Show Sign where show = unpack . _sign

-- errors ----------------------------------------------------------------------

-- | Trying to lookup a non-existent 'Sign'
newtype LookupError = NoSuchSign Sign
makeClassyPrisms ''LookupError

-- | Trying to overwrite a pre-existing 'Sign'
newtype InsertError = DuplicateSign Sign
makeClassyPrisms ''InsertError

-- | Either an lookup or insertion error
data SignError
  = SignLookupError LookupError
  | SignInsertError InsertError
makeClassyPrisms ''SignError

instance Show LookupError where
  show (NoSuchSign s) = "Could not find sign: " <> show s

instance Show InsertError where
  show (DuplicateSign s) = "Sign already exists: " <> show s

instance Show SignError where
  show (SignLookupError e) = show e
  show (SignInsertError e) = show e

instance Exception LookupError
instance Exception InsertError
instance Exception SignError

instance AsLookupError SignError where _LookupError = _SignLookupError
instance AsInsertError SignError where _InsertError = _SignInsertError

-- classes ---------------------------------------------------------------------
