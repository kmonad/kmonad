module Data.CChan
  ( -- * The 'Captured' wrapper
    -- $capt
    Captured
  , release

    -- * The 'CChan' channel implementation
    -- $cchan
  , CChan
  , withCChan
  , mkCChan
  , copyStream
  , captureStream
  , write

    -- * Polymorphic 'read' interface
    -- $poly
  , read

  )
where

import Prelude

import Control.Concurrent.Chan.Unagi

import KMonad.Util


--------------------------------------------------------------------------------
-- $capt
--
-- When a 'CChan' is captured, it immediately interrupts all broadcast reads.
-- Any event that arrives to the 'CChan' from then on will be immediately send
-- to 'Captured' interface. This interface allows reading new events, and
-- provides the ability to release the stream again to allow normal broadcasting
-- to continue.

-- | A captured datastream, providing the possiblity to 'read' and 'release'
data Captured a = Captured
   { _readCap :: IO a
   , _release :: IO ()
   }

readCap :: MonadIO m => Captured a -> m a
readCap = liftIO . _readCap

-- | Release the stream-capture, resuming normal broadcast (or allowing a
-- waiting capture request to get control of the stream).
release :: MonadIO m => Captured a -> m ()
release = liftIO . _release


--------------------------------------------------------------------------------
-- $ cchan
--

-- | The 'CChan' datatype, representing a basic 'Chan'-style concurrent
-- processing queue with the additional functionality that the output stream can
-- be captured and returned (bypassing any normal reads in the process).
data CChan a = CChan
  { _chIn       :: !(InChan  a)         -- ^ Write-end of CChan
  , _chMidIn    :: !(InChan  a)         -- ^ Write here to broadcast
  , _chOut      :: !(OutChan a)         -- ^ Broadcast output
  , _capEnd     :: !(MVar (a -> IO ())) -- ^ Capture `socket`
  }
makeLenses ''CChan

-- | Run an action in the context of an initialized 'CChan'. Once the action
-- finishes, the 'CChan' is properly cleaned up again.
withCChan :: HasLogFunc e => (CChan b -> RIO e a) -> RIO e a
withCChan f = do
  (i, mo) <- liftIO newChan
  (mi, o) <- liftIO newChan
  cap     <- newEmptyMVar

  let copyOver = do
      e <- liftIO . readChan $ mo
      tryReadMVar cap >>= \case
        Nothing -> liftIO $ writeChan mi e
        Just c  -> liftIO $ c e

  withThread "cchan:internal" copyOver . f $ CChan i mi o cap

-- | withCChan wrapped in a 'ContT' for use in initialization code.
mkCChan :: HasLogFunc e => ContT a (RIO e) (CChan b)
mkCChan = ContT withCChan


-- | Inject a new thing into the front of the 'CChan'
write :: MonadUnliftIO m => a -> CChan a -> m ()
write a = liftIO . flip writeChan a . view chIn

-- | Perform a blocking read of a 'CChan'
readCC :: MonadUnliftIO m => CChan a -> m a
readCC = liftIO . readChan . view chOut

-- | Get a copy of the output stream
copyStream :: MonadUnliftIO m => CChan a -> m (m a)
copyStream cc = do
  mo <- liftIO . dupChan $ cc^.chMidIn
  pure . liftIO . readChan $ mo

-- | Capture the stream, returning an object that can both read and release the
-- stream. If the stream is already captured, this call will block until the
-- stream is released by its previous capturer.
captureStream :: MonadUnliftIO m => CChan a -> m (Captured a)
captureStream cc = do
  (i, o) <- liftIO newChan
  putMVar (cc^.capEnd) (writeChan i)
  pure $ Captured (readChan o) (void . takeMVar $ cc^.capEnd)

--------------------------------------------------------------------------------
-- $poly
--
-- Provide a polymorphic uniform `read` interface to both the 'Capture' and
-- 'CChan' objects.
--

-- | Both 'CChan' and 'Captured' are instances of this class, allowing both to
-- simply be 'read' from.
class HasRead f a where
  read :: forall m. MonadUnliftIO m => f a -> m a

instance HasRead CChan    a where read = readCC
instance HasRead Captured a where read = readCap
