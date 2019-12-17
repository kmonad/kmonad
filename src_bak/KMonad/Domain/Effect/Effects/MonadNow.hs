module KMonad.Domain.Effect.Effects.MonadNow
  ( MonadNow(..)
  , nowIO
  , withNow
  )
where

import Control.Lens
import Control.Monad.IO.Class
import Data.Time.Clock.System (getSystemTime)
import KMonad.Core.Time

-- | This effect allows access to the current time.
class Monad m => MonadNow m where
  now :: m Time

instance MonadNow IO where
  now = nowIO

-- | Get the current time using IO
nowIO :: MonadIO m => m Time
nowIO = view (from isoSystemTime) <$> liftIO getSystemTime

-- | A 'withNow' helper function to generate MonadNow calculations
withNow :: MonadNow m => (Time -> a) -> m a
withNow f = f <$> now
