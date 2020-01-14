module KMonad.Daemon.Callback

where

import Prelude

import Data.Semigroup (Any(..))
import Data.Unique
import KMonad.Keyboard
import KMonad.Util

import KMonad.Button.Action
import qualified RIO.HashMap as M


--------------------------------------------------------------------------------
-- $cb

data Callback
  = Next KeyPred KeyHandler
  | Within Unique KeyPred KeyHandler

pred :: Getter Callback KeyPred
pred = to $ \case
  (Next p _)     -> p
  (Within _ p _) -> p

hdlr :: Getter Callback KeyHandler
hdlr = to $ \case
  (Next _ h)     -> h
  (Within _ _ h) -> h

runCallback :: MonadButton m => Callback -> KeyEvent -> m KeyCaptured
runCallback c e = runKeyPred (c^.pred) e >>= runKeyHandler (c^.hdlr)


-- data Callback = Callback
--   { _pred :: (CBDomain -> Bool)
--   , _act  :: (CBDomain -> IO Ac.KeyCaptured)
--   , _tag  :: Unique
--   }
-- makeLenses ''Callback

-- mkCallback :: MonadUnliftIO m
--   => (CBDomain -> Bool)
--   -> (CBDomain -> m Ac.KeyCaptured)
--   -> m Callback
-- mkCallback p a = withRunInIO $ \u -> do
--   Callback p (u . a) <$> liftIO newUnique

-- runCallback :: Callback -> CBDomain -> CBResult
-- runCallback cb x@(CBTimer t) =
--   if cb^.tag == t
--   then let io = cb^.act x


--------------------------------------------------------------------------------
-- $cbsig


-- data CbStore = CbStore
--   { _cbMap:: TVar (M.HashMap Unique Callback)
--   }
-- makeClassy ''CbStore

-- registerSTM :: CbStore -> Callback -> STM ()
-- registerSTM s c = modifyTVar' (s^.cbMap) $ M.insert (c^.tag) c

-- unregisterSTM :: CbStore -> Unique -> STM ()
-- unregisterSTM s t = modifyTVar' (s^.cbMap) $ M.delete t

-- register :: HasCbStore e => Callback -> RIO e ()
-- register c = view cbStore >>= \s -> atomically $ registerSTM s c

-- unregister :: HasCbStore e => Unique -> RIO e ()
-- unregister t = view cbStore >>= \s -> atomically $ unregisterSTM s t

-- checkKeyEvent :: KeyEvent -> [Callback] -> (Any, [Callback], IO ())
-- checkKeyEvent = undefined


--------------------------------------------------------------------------------
-- $hooks

-- catchNext :: HasCbStore e => Ac.KeyPred -> Ac.KeyHandler -> RIO e ()
-- catchNext kp kh = register =<< mkCallback p h
--   where
--     p = onKey
--     h = undefined
