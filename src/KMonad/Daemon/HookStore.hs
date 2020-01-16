{-# LANGUAGE UndecidableInstances #-}
module KMonad.Daemon.HookStore
  ( HookStore
  , mkHookStore
  , HasHookStore
  , hookStore
  , hookNext
  , hookWithin
  , pull
  )
where

import Prelude

import Data.Unique
import KMonad.Keyboard
import KMonad.Util

import KMonad.Action hiding (hookNext, hookWithin, HookFun)
import qualified RIO.HashMap as M

--------------------------------------------------------------------------------
-- $kh

-- | An internal keyhandler object that contains a predicate and an action
data KH = KH
  { _pred :: HookPred
  , _act  :: Match -> IO () }
makeLenses ''KH

-- | Turn any MonadUnliftIO action into a KH
mkKH :: MonadUnliftIO m => HookPred -> (Match -> m ()) -> m KH
mkKH p a = withRunInIO $ \u -> pure $ KH p (u . a)

-- | The result-type of running a KH
newtype KHRes = KHRes (Bool, IO ())
makeWrapped ''KHRes

-- | Make KHRes a Monoid with 'or' on the bool and sequencing of IO actions
instance Semigroup KHRes where
  (KHRes (ab, aio)) <> (KHRes (bb, bio)) = KHRes (ab || bb, aio >> bio)
instance Monoid KHRes where
  mempty = KHRes (False, pure ())

-- | Run a KH on a KeyEvent, returning the KHRes
runKH :: KH -> KeyEvent -> KHRes
runKH kh e = do
  let m = runPred (kh^.pred) e
  KHRes $ (m^.caught, kh^.act $ m)

--------------------------------------------------------------------------------
-- $env

-- TODO: Probably can make 'nextHooks' an IO-ref

-- | The different 'inherited' components of the HookStore runtime environment
data HookStore = HookStore
  { _injectSrc  :: TMVar KeyEvent
  , _injectTmr  :: TMVar Unique
  , _nextHooks  :: TVar [KH]
  , _timerHooks :: TVar (M.HashMap Unique KH)
  }
makeClassy ''HookStore

-- | Initialize a new 'HookStore' environment
mkHookStore' :: RIO e HookStore
mkHookStore' = do
  isr <- atomically $ newEmptyTMVar
  itr <- atomically $ newEmptyTMVar
  nxh <- atomically $ newTVar []
  trh <- atomically $ newTVar M.empty
  pure $ HookStore isr itr nxh trh

mkHookStore :: ContT r (RIO e) HookStore
mkHookStore = lift mkHookStore'


--------------------------------------------------------------------------------
-- $loop
--
-- All the code dealing with getting events through the 'HookStore' context and
-- running hooks on events.

-- | Print out information about hooks and matches
debugReport :: HasHookStore e => KeyEvent -> RIO e Text
debugReport e = do
  let fOne kh = " - " <> textDisplay (kh^.pred)
             <> ": "  <> textDisplay (runPred (kh^.pred) e)

  -- Generate the report for the 'NEXT' hooks
  nhs <- view nextHooks >>= \n -> atomically $ readTVar n
  let ntxt = if null nhs
        then ["Running no <NEXT>  hooks"]
        else "Running <NEXT> hooks: ":(map fOne nhs)

  -- Generate the report for the 'TIMER' hooks
  ths <- view timerHooks >>= \t -> M.elems <$> (atomically $ readTVar t)
  let ttxt = if null ths
        then ["Running no <TIMER> hooks"]
        else "Running <TIMER> hooks: ":(map fOne ths)

  pure . unlines $ ntxt <> ttxt


-- | Run all hooks on the provided keyevent.
--
-- NOTE: The hooks are run very differently:
--
-- - A `next` hook is run only once, if it matches, its 'Match' action is run,
--   if it doesn't match, its 'NoMatch' action is run. Either way, the hook is
--   removed from the 'HookStore' action is run. Either way, the hook is removed
--   from the 'HookStore'
--
-- - A `timer` hook is checked against any KeyEvent as long as it remains in the
--   'HookStore'. The moment a timer hook matches an input, its 'Match' action is
--   run, and it is removed. However, when a timer doesn't match on a KeyEvent,
--   no action is performed, but the hook remains in the store. The way a
--   `timer` hook registers a 'NoMatch' is when its timer-event occurs when it
--   is still in the 'HookStore'.
--  
runHooks :: (HasLogFunc e, HasHookStore e) => KeyEvent -> RIO e (Maybe KeyEvent)
runHooks e = do
  logDebug . display =<< debugReport e

  th  <- view timerHooks
  nh  <- view nextHooks

  (KHRes (mtch, io))  <- atomically $ do
    nRes <- foldMap (flip runKH e) <$> swapTVar nh []
    tRes <- runTimers th
    pure $ nRes <> tRes

  liftIO io
  pure $ if mtch then Nothing else Just e

  where
    runTimers th = do
      (r, m') <- M.foldrWithKey doOne mempty <$> readTVar th
      writeTVar th m'
      pure r

    doOne k f acc = let r = runKH f e in
      if r^._Wrapped._1
      then (r, M.empty)                 <> acc -- If matched, remove callback
      else (mempty (), M.singleton k f) <> acc -- If no match, do nothing

-- | Pull 1 event from the action we use to generate KeyEvents. If that action
-- is not caught by any callback, then return it (otherwise return Nothing). At
-- the same time, stay receptive to any 'TimerEvents' and handle them as they
-- occur. No 2 events will ever be processed at the same time.
step :: (HasLogFunc e, HasHookStore e)
  => RIO e KeyEvent
  -> RIO e (Maybe KeyEvent)
step pullSrc = do
  iSrc <- view injectSrc
  iTmr <- view injectTmr

  -- Asynchronously start a thread that will write 1 event from the event-source
  -- to the inject-point TMVar
  void . async $ do
    e <- pullSrc
    atomically . putTMVar iSrc $ e

  -- Handle any timer event first, and then try to read from the source
  let next = (Left <$> takeTMVar iTmr) `orElse` (Right <$> takeTMVar iSrc)

  -- Keep taking and cancelling timers until we encounter a key event, then run
  -- the hooks on that event.
  let read = atomically next >>= \case
        Left  t -> cancelTimer t >> read
        Right e -> runHooks e
  read

-- | Keep stepping until we succesfully get a KeyEvent
pull :: (HasLogFunc e, HasHookStore e)
  => RIO e KeyEvent
  -> RIO e KeyEvent
pull pullSrc = step pullSrc >>= maybe (pull pullSrc) pure

--------------------------------------------------------------------------------
-- $hooks
--
-- All the code dealing with adding and removing hooks from the 'HookStore'
-- environment.

-- | Implementation of 'hookNext' from "KMonad.Action".
--
-- This adds a hook to the 'nextHooks' list in 'HookStore', where it will be called
-- on the next 'KeyEvent' to occur.
hookNext :: (HasLogFunc e, HasHookStore e)
  => HookPred
  -> (Match -> RIO e ())
  -> RIO e ()
hookNext p a = do
  logDebug $ "Registering <NEXT>  hook: " <> display p
  st <- view nextHooks
  cb <- mkKH p a
  atomically $ modifyTVar st (cb:)

-- | Implementation of 'hookWithin' from "KMonad.Action".
--
-- Inserts a KH into the 'timerHooks' hashmap in the 'HookStore'. It also
-- asynchronously starts a thread that will signal when the timer has expired.
hookWithin :: (HasLogFunc e, HasHookStore e)
  => Milliseconds
  -> HookPred
  -> (Match -> RIO e ())
  -> RIO e ()
hookWithin ms p a = do
  logDebug $ "Registering <TIMER> hook: " <> display p <> ", " <> display ms <> "ms"
  st <- view timerHooks
  kh <- mkKH p a
  tg <- liftIO newUnique
  atomically $ modifyTVar st (M.insert tg kh)
  void . async $ do
    threadDelay $ 1000 * (fromIntegral ms)
    signalTimeout tg

-- | Send a signal to the 'HookStore' indicating that a timer hook has expired.
signalTimeout :: HasHookStore e => Unique -> RIO e ()
signalTimeout t = do
  v <- view injectTmr
  atomically $ putTMVar v t

-- | Try to cancel a hook stored in 'timerHooks'. If it doesn't exist, do
-- nothing. If it does, remove it from the map and call it on 'NoMatch'.
cancelTimer :: HasHookStore e => Unique -> RIO e ()
cancelTimer t = do
  th <- view timerHooks
  join . atomically $ do
    m <- readTVar th
    let cncl f = do
          modifyTVar th (M.delete t)
          pure . liftIO . (f^.act) $ NoMatch
    maybe (pure $ pure ()) cncl $ M.lookup t m





