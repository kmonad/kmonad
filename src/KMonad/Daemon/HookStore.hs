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

import KMonad.Button.Action hiding (hookNext, hookWithin, HookFun)
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
  let m = kh^.pred $ e
  KHRes $ (m^.caught, kh^.act $ m)

--------------------------------------------------------------------------------
-- $env

-- | The different components of the HookStore runtime environment
data HookStore = HookStore
  { _pullSrc    :: IO KeyEvent
  , _injectSrc  :: TMVar KeyEvent
  , _injectTmr  :: TMVar Unique
  , _nextHooks  :: TVar [KH]
  , _timerHooks :: TVar (M.HashMap Unique KH)
  }
makeClassy ''HookStore

-- | Initialize a new 'HookStore' environment
mkHookStore' :: MonadUnliftIO m => m KeyEvent -> m HookStore
mkHookStore' src = withRunInIO $ \u -> do
  isr <- atomically $ newEmptyTMVar
  itr <- atomically $ newEmptyTMVar
  nxh <- atomically $ newTVar []
  trh <- atomically $ newTVar M.empty
  pure $ HookStore (u src) isr itr nxh trh

mkHookStore :: MonadUnliftIO m => m KeyEvent -> ContT r m HookStore
mkHookStore = lift . mkHookStore'


--------------------------------------------------------------------------------
-- $loop
--
-- All the code dealing with getting events through the 'HookStore' context and
-- running hooks on events.

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
runHooks :: HasHookStore e => KeyEvent -> RIO e (Maybe KeyEvent)
runHooks e = do
  th  <- view timerHooks
  nh  <- view nextHooks

  -- FIXME: Delete me when done debugging
  -- traceShowIO =<< do
  --   cbs <- (atomically $ readTVar nh :: RIO e [KH])
  --   let khs = map (flip runKH e) cbs
  --   pure $ map (\(KHRes (b, _)) -> b) khs

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
step :: HasHookStore e => RIO e (Maybe KeyEvent)
step = do
  iSrc <- view injectSrc
  iTmr <- view injectTmr

  -- Asynchronously start a thread that will write 1 event from the event-source
  -- to the inject-point TMVar
  void . async $ do
    e <- liftIO =<< view pullSrc
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
pull :: HasHookStore e => RIO e KeyEvent
pull = step >>= maybe pull pure

--------------------------------------------------------------------------------
-- $hooks
--
-- All the code dealing with adding and removing hooks from the 'HookStore'
-- environment.

-- | Implementation of 'hookNext' from "KMonad.Button.Action".
--
-- This adds a hook to the 'nextHooks' list in 'HookStore', where it will be called
-- on the next 'KeyEvent' to occur.
hookNext :: HasHookStore e
  => HookPred
  -> (Match -> RIO e ())
  -> RIO e ()
hookNext p a = do
  st <- view nextHooks
  cb <- mkKH p a
  atomically $ modifyTVar st (cb:)

-- | Implementation of 'hookWithin' from "KMonad.Button.Action".
--
-- Inserts a KH into the 'timerHooks' hashmap in the 'HookStore'. It also
-- asynchronously starts a thread that will signal when the timer has expired.
hookWithin :: HasHookStore e
  => Milliseconds
  -> HookPred
  -> (Match -> RIO e ())
  -> RIO e ()
hookWithin ms p a = do
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





