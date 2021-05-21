module KMonad.Util.Ctx
  ( Ctx
  , mkCtx
  , runCtx
  , runCtx_
  , nest
  , launch_
  , around
  )
where

import KMonad.Prelude

import Control.Monad.Cont
import UnliftIO.Async (wait)

{- Context notes:

We often use this type of calling signature:
  withAbc :: AbcCfg -> (AbcEnv -> m a) -> m a

Use some config to establish the context of AbcEnv existing, then run a function
in that context, and return the result. An example of this is the
bracket-pattern, where we acquire a resource, and when we finish, we want to
ensure we clean up that resource.

Nearly all of the code in App uses these types of constructs.

Furthermore, in KeyIO we want to nest a context. Simplified, we want to be able
to treat a "[(GetKey -> m a) -> m a]" as a "([GetKey] -> m a) -> m a"

This is essentially just a Continuation monad, but without using any of the
complicated callCC mechanisms. I often find trying to comprehend documentation
harder than experimenting, so here is a simplified Cont, attempting to ease
context usage.

This module is arguably redundant and we might be better off just using ContT,
but that was such a brainbender to get my head into, that this presentation
seems more comprehensible to me.

TODO: Put me somewhere better

-}

-- | The 'Ctx' type, something that can run a computation that needs an 'a' by
-- generating an 'a' somehow.
newtype Ctx r m a = Ctx { unCtx :: ContT r m a }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadIO)

-- instance (MonadIO m) => MonadIO (Ctx r m a) where
--   lift m =

-- | Create a new 'Ctx' from a function that calls a continuation somehow.
mkCtx :: ((a -> m r) -> m r) -> Ctx r m a
mkCtx = Ctx . ContT

-- | Run a computation that needs an 'a' in the 'Ctx' that generates said 'a'
runCtx :: Ctx r m a -> (a -> m r) -> m r
runCtx = runContT . unCtx

-- | Run a computation that in a 'Ctx', but without any arguments being passed.
runCtx_ :: Ctx r m a -> m r -> m r
runCtx_ ctx = runContT (unCtx ctx) . const

-- | Turn a list of contexts into a context that nests all the contexts
nest :: [Ctx r m a] -> Ctx r m [a]
nest = traverse id

-- | Modify some action so that some command is running in the background,
-- forever, untill that action terminates.
launch_ :: UIO m => m a -> Ctx r m ()
launch_ go = mkCtx $ \f -> withAsync (forever go) (const $ f ())

-- | Do something before and after some action, even on exception.
around :: UIO m => m b -> m c -> Ctx r m ()
around before after = mkCtx $ \f -> bracket_ before after (f ())

-- -- | Like `launch`
-- launch_ :: HasLogFunc e
--   => Text    -- ^ The name of this process (for logging)
--   -> RIO e a -- ^ The action to repeat forever
--   -> ContT r (RIO e) ()
-- launch_ n a = ContT $ \next -> withLaunch_ n a (next ())
