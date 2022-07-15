-- |

module K.Shell.Error where

import K.Shell.Initial
import K.Initial.Parsing (ParseError, AsParseError(..))
import Control.Monad.Catch
import qualified Control.Exception.Lens as Exc

data ShellError
  = ShellLocaleError LocaleError
  | ShellParseError  ParseError
makeClassyPrisms ''ShellError

instance Show ShellError where
  show (ShellLocaleError e) = show e
  show (ShellParseError e) = show e

instance Exception ShellError
instance AsShellError SomeException where _ShellError = Exc.exception
instance AsLocaleError ShellError where _LocaleError = _ShellLocaleError
instance AsParseError ShellError where __ParseError = _ShellParseError

withShellHandler :: MonadCatch m => Ctx r m ()
withShellHandler = ContT $ \f -> handleShellError $ f ()

-- | The outermost error-catching mechanism in KMonad
--
-- This currently does nothing except rethrow the error.
handleShellError :: MonadCatch m => m a -> m a
handleShellError = Exc.handling _ShellError handle
  where handle = excThrowing _ShellError

-- |
throwLeft :: (MonadIO m) => Either SomeException a -> m a
throwLeft = either throwIO pure
