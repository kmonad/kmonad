{-

The 'Coaround' component solves the Problem of Keyboard firmware implementing tap-macros.
We want to remap physical keys. So if a key press results in multiple actions we catch those
and emit a single press / release event.

Those keys need to be configured.

-}
module KMonad.Model.Coaround
  ( Coaround
  , CoaroundKey(..)
  , mkCoaround
  , pull
  )
where

import KMonad.Prelude
import KMonad.Util
import KMonad.Keyboard
import KMonad.Model.EventSrc

import Data.Unique

import RIO.Seq (Seq(..), (><))
import qualified RIO.Seq  as Seq
import qualified RIO.NonEmpty as N
import qualified RIO.HashMap as M

data CoaroundKey = CoaroundKey Keycode (NonEmpty Keycode)

data Coaround = Coaround
  { eventSrc :: EventSrc IO -- ^ Where we get our 'KeyEvent's from
  , _coarounds :: [CoaroundKey] -- ^ All recognized coaround keys. No sequence should be a prefix of another
  , _coaroundLasts :: HashMap Keycode Keycode -- ^ A map to map the key release based on the last entry in coarounds
  , _codelay :: Milliseconds -- ^ How much time may pass between two keys which are part of the same coaroundkey
  , _blockedKCs :: IORef (Seq Keycode) -- ^ Blocked keycodes which may be part of a coaround key
  , _rerunKCs :: TVar (Seq Keycode) -- ^ Previously blocked keycodes which need to be rerun
  , _inProcessCAs :: IORef [CoaroundKey] -- ^ Internal state of possible continuations of a coaround sequence
  , _injectTmr  :: TMVar Unique  -- ^ Used to signal timeouts
  , _pendingTmr :: TVar Unique
  }
makeLenses ''Coaround

mkCoaround' :: MonadUnliftIO m => Milliseconds -> [CoaroundKey] -> EventSrc m -> m Coaround
mkCoaround' cd cas s = withRunInIO $ \u -> do
  let cals = M.fromList $ cas <&> \(CoaroundKey r seq) -> (N.last seq, r)
  buf <- newIORef mempty
  rerun <- newTVarIO mempty
  ipcas <- newIORef []
  itr <- newEmptyTMVarIO
  pending <- newTVarIO =<< newUnique
  pure $ Coaround (unliftESrc u s) cas cals cd buf rerun ipcas itr pending

mkCoaround :: MonadUnliftIO m => Milliseconds -> [CoaroundKey] -> EventSrc m -> ContT r m Coaround
mkCoaround cd cas = lift . mkCoaround' cd cas

advance :: Keycode -> [CoaroundKey] -> Either [CoaroundKey] Keycode
advance _ [] = Left []
advance kc (CoaroundKey r (kc' :| rest) : cas)
  | kc == kc' = case rest of
    [] -> Right r -- This coaround completed. Emit
    (x : xs) -> case advance kc cas of
      Right r' -> Right r' -- Other coaround completed
      Left cas' -> Left $ CoaroundKey r (x :| xs) : cas'
advance kc (CoaroundKey _ (_ :| []) : cas) = advance kc cas -- This sequence does not match
advance kc (CoaroundKey r (_ :| x : xs) : cas) = -- Skip current entry in the sequence
  advance kc (CoaroundKey r (x :| xs) : cas)

step :: HasLogFunc e => Coaround -> KeyEvent -> RIO e (Maybe KeyEvent)
step c (KeyEvent Press kc) = do
  cas <- readIORef (c^.inProcessCAs) <&> \case
    [] -> c^.coarounds
    cas -> cas

  case advance kc cas of
    -- Not a CA at index 0 in blockedKEs
    -- It could still contain another start of a sequence
    -- requiring unblocking them individually.
    -- We ignore this edge case for now
    Left [] -> do
      writeIORef (c^.inProcessCAs) []
      blocked <- readIORef (c^.blockedKCs)
      atomically $ modifyTVar (c^.rerunKCs) (>< (blocked |> kc))
      writeIORef (c^.blockedKCs) mempty
      pure Nothing
    Left cas' -> do
      writeIORef (c^.inProcessCAs) cas'
      modifyIORef (c^.blockedKCs) (|> kc)
      tag <- liftIO newUnique
      atomically $ writeTVar (c^.pendingTmr) tag
      void . async $ do
        threadDelay $ 1000 * fromIntegral (c^.codelay)
        atomically $ putTMVar (c^.injectTmr)  tag
      pure Nothing
    Right rkc -> do
      writeIORef (c^.inProcessCAs) []
      writeIORef (c^.blockedKCs) mempty
      pure $ Just (KeyEvent Press rkc)
step c ke@(KeyEvent Release kc) = case M.lookup kc (c^.coaroundLasts) of
  Just r -> pure $ Just (KeyEvent Release r)
  Nothing -> pure $ Just ke

pull :: (HasLogFunc e)
  => Coaround
  -> EventSrc (RIO e)
pull c@Coaround{eventSrc = EventSrc{tryESrc, postESrc}} = EventSrc
  { tryESrc = (Left . Left <$> tryRerun) `orElse` (Left . Right <$> takeTMVar (c^.injectTmr)) `orElse` (Right <$> tryESrc)
  , postESrc = \case
    Left (Left kc) -> pure $ Just (KeyEvent Press kc)
    Left (Right t) -> runTimeout t $> Nothing
    Right e -> liftIO (postESrc e) >>= maybe (pure Nothing) (step c) -- We caught a real event
  }
 where
  tryRerun = do
    rerun <- readTVar (c^.rerunKCs)
    case rerun of
      Seq.Empty -> retrySTM
      (kc :<| rerun') -> kc <$ writeTVar (c^.rerunKCs) rerun'
  runTimeout t = do
    t' <- atomically $ readTVar (c^.pendingTmr)
    if t /= t' then pure ()
    else do
      writeIORef (c^.inProcessCAs) []
      blocked <- readIORef (c^.blockedKCs)
      atomically $ modifyTVar (c^.rerunKCs) (>< blocked)
      writeIORef (c^.blockedKCs) mempty
