{-|
Module      : KMonad.Core.BracketIO
Description : Helper type to define bracketed IO
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

The 'BracketIO' type wraps the acquisition, release, and use functions of a
resource in a single datatype. The only way to access the resource in the
'BracketIO' is through the 'withBracketIO' function, ensuring that proper
acquisition and cleanup always occurs.

This could probably just be done with /resourcet/, but I needed something simple
and small that I could grok.

-}
module KMonad.Core.BracketIO
  ( -- * BracketIO
    -- $bio
    BracketIO
  , mkBracketIO
  , mkBracketIOM
  , withBracketIO
  )
where

import KMonad.Prelude

--------------------------------------------------------------------------------
-- $bio
--
-- The basic 'BracketIO' type, its constructor, and its /context-function/

-- | The generalized data type for IO actions that can only exist in the context
-- of being bracketed by some /acquire/ and /release/ behavior.
data BracketIO a = forall b r. BracketIO
  { _open  :: IO r
  , _close :: r -> IO b
  , _use   :: r -> a }

-- | Create a new 'BracketIO' datatype that can be acquired and released
mkBracketIO ::
     IO r        -- ^ The action to acquire the resource
  -> (r -> IO b) -- ^ The action to release the resource
  -> (r -> a)    -- ^ The action to perform with the resource
  -> BracketIO a -- ^ The 'BracketIO' datatype
mkBracketIO = BracketIO

-- | Create a 'BracketIO' from some monad 'm'
mkBracketIOM :: MonadUnliftIO m
  => m r
  -> (r -> m b)
  -> (r -> a)
  -> m (BracketIO a)
mkBracketIOM o c u = do
  f <- askUnliftIO
  pure $ BracketIO (unliftIO f $ o) (unliftIO f . c) u



-- | Run an action that requires a managed /a/ by bracketting it with acquiring
-- and releasing the resource.
withBracketIO :: MonadUnliftIO m => BracketIO a -> (a -> m b) -> m b
withBracketIO BracketIO{ _open=o, _close=c, _use=u } go = do
  bracket (liftIO o) (liftIO . c) (\a -> (go $ u a))
