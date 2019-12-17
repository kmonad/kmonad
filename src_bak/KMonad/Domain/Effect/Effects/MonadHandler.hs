module KMonad.Domain.Effect.Effects.MonadHandler
  ( MonadHandler(..)
  )
where

import KMonad.Core

-- | This typeclass is simply used to inject a 'KeyEvent' handling function into
-- domain-code. Any concrete API needs to implement some method of dealing with
-- 'KeyEvent's and plug it into Domain code by defining an instance of this
-- class.
class Monad m => MonadHandler m where
  handle :: KeyEvent -> m ()
