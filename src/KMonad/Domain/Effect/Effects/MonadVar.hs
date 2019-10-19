module KMonad.Domain.Effect.Effects.MonadVar
  ( MonadVar(..)
  , Var
  , runVar, getV, putV, swapVar, readVar
  )
where

import Control.Monad.IO.Class
import Control.Concurrent.MVar

-- This effect allows maintaining a piece of thread-safe, local state. This is
-- necessary for certain button operations that need to keep track of what state
-- they exist in. This is a very shallow abstraction over MVar, so note that
-- getting and putting values can block.

-- | A reference to some piece of local state
newtype Var a = Var { unVar :: MVar a }

-- | Create an action that uses a 'Var' to manage local state
runVar :: (MonadIO m) => a -> (Var a -> b) -> m b
runVar a f = do
  v <- liftIO . newMVar $ a
  return $ f (Var v)

-- | Simple MonadIO implementation of getVar
getV :: MonadIO m => Var a -> m a
getV v = liftIO $ takeMVar $ unVar v

-- | Simple MonadIO implementation of putVar
putV :: MonadIO m => a -> Var a -> m ()
putV a v = liftIO $ putMVar (unVar v) a

-- | The MonadVar class that allows the creation and manipulation of local state
class Monad m => MonadVar m where
  getVar :: Var a -> m a
  putVar :: a -> Var a -> m ()

-- | Swap the value in a Var and return the old one
swapVar :: MonadVar m => a -> Var a -> m a
swapVar a v = getVar v >>= \old -> putVar a v >> return old

-- | Read the value from a Var without taking it
readVar :: MonadVar m => Var a -> m a
readVar v = getVar v >>= \x -> putVar x v >> pure x
