-- |

module K.Keyboard.Locale
  ( Locale(..)
  , HasLocale(..)

  , LocaleError
  , AsLocaleError(..)

  , CanLocale

  , lookupCode
  , lookupRap
  ) where

import K.Keyboard.Initial
import K.Gesture

import qualified Control.Monad.Error.Lens as Err

-- basic types -----------------------------------------------------------------

-- | Bindings of names to keycodes and names to gestures of keycodes
data Locale = Locale
  { _namedCodes :: NameMap Keycode
    -- ^ A collection of name-to-keycode correspondences
  , _namedRaps  :: NameMap Rap
    -- ^ A collection of name-to-gesture correspondences
  } deriving (Eq, Show)
makeClassy ''Locale

-- error -----------------------------------------------------------------------

data LocaleError
  = MissingKey Keyname
  | MissingRap Name
makeClassyPrisms ''LocaleError

instance Show LocaleError where
  show (MissingKey n)
    = "Unknown keyname: " <> unpack n
  show (MissingRap n)
    = "Unknown gesture: " <> unpack n

instance Exception LocaleError
instance AsLocaleError SomeException where _LocaleError = _SomeException

-- shorthand -------------------------------------------------------------------

-- | Things that have access to a Locale and might throw LocaleErrors
type CanLocale v r m = ( HasLocale v, AsLocaleError r
                       , MonadReader v m, MonadError r m)

-- ops -------------------------------------------------------------------------

-- | Lookup a keycode by its name
lookupCode :: CanLocale v r m => Keyname -> m Keycode
lookupCode n = view (namedCodes.at n)
  >>= maybe (errThrowing _MissingKey n) pure

-- | Lookup a rap by its name
lookupRap :: CanLocale v r m => Name -> m Rap
lookupRap n = view (namedRaps.at n)
  >>= maybe (errThrowing _MissingRap n) pure
