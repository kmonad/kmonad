-- |

module K.Layers.KbdFile.Initial
  ( KExpr(..)
  , Loc

  , KbdError(..)
  , AsKbdError(..)

  , tryP

  , module Data.Char
  , module K.Initial
  , module K.Initial.Parsing
  , module K.Keyboard
  , module K.Layers.Initial
  )
where

import K.Initial
import K.Initial.Parsing
import K.Keyboard
import K.Layers.Initial

import Data.Char

import K.Shell -- DEBUG

import qualified Control.Exception.Lens as Exc

-- basic types -----------------------------------------------------------------

-- | All the different klang top-level expressions
data KExpr c
  = KSrc SrcName [c]                         -- ^ A `defsrc` expression
  | KLayer LayerName (Maybe SrcName) [But c] -- ^ A `deflayer` expression
  | KPairs LayerName [(c, But c)]      -- ^ A `defpairs` expression
  | KAlias [(ButtonName, But c)]             -- ^ A `defalias` expression
  deriving (Eq, Show, Functor)

type Loc e m = (MonadReader e m, HasLocale e)

-- error -----------------------------------------------------------------------

data KbdError
  = KbdParseError ParseError
  | KbdLocaleError LocaleError
makeClassyPrisms ''KbdError

instance Show KbdError where
  show (KbdParseError e)
    = "Parse error in kbd-file: " <> show e
  show (KbdLocaleError e)
    = "Loclae error in kbd-file: " <> show e

instance Exception KbdError
instance AsKbdError SomeException where _KbdError = _SomeException
instance AsParseError KbdError where __ParseError = _KbdParseError
instance AsLocaleError KbdError where _LocaleError = _KbdLocaleError

-- ops -------------------------------------------------------------------------


-- DEBUG -----------------------------------------------------------------------

tryP :: Show a => ParserT CfgM a -> Text -> IO ()
tryP p t = inCtx (testIvkM "") . inCtx cfgM $ do
  parseT p t >>= \case
    Left e -> excThrowing __ParseError e
    Right a -> pPrint a
