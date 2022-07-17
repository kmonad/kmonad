{-| A small utility library for working with named items. -}
module K.Initial.Util.Sign where

import K.Initial

-- basic sign type -------------------------------------------------------------

newtype Sign = Sign { _sign :: Text }

instance Show Sign where show = unpack . _sign

-- errors ----------------------------------------------------------------------

-- | Trying to lookup a non-existent 'Sign'
newtype LookupError s = NoSuchSign s
makeClassyPrisms ''LookupError

-- | Trying to overwrite a pre-existing 'Sign'
newtype InsertError s = DuplicateSign s
makeClassyPrisms ''InsertError

-- | Either a lookup or insertion error
data SignError s
  = SignLookupError (LookupError s)
  | SignInsertError (InsertError s)
makeClassyPrisms ''SignError

instance Show s => Show (LookupError s) where
  show (NoSuchSign s) = "Could not find sign: " <> show s

instance Show s => Show (InsertError s) where
  show (DuplicateSign s) = "Sign already exists: " <> show s

instance Show s => Show (SignError s) where
  show (SignLookupError e) = show e
  show (SignInsertError e) = show e

instance (Show s, Typeable s) => Exception (LookupError s)
instance (Show s, Typeable s) => Exception (InsertError s)
instance (Show s, Typeable s) => Exception (SignError s)

instance AsLookupError (SignError s) s where _LookupError = _SignLookupError
instance AsInsertError (SignError s) s where _InsertError = _SignInsertError

-- classes ---------------------------------------------------------------------

-- newtype Signed a = Signed { M.HashMap  }

-- api  ------------------------------------------------------------------------

{- The functions I am working towards -}

addAliases :: At m => [(IxValue m, [IxValue m])] -> m -> m
addAliases = undefined
