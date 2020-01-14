module KMonad.Daemon.Dispatch
  ( Dispatch
  , HasDispatch
  , dispatch
  , mkDispatch
  , pull
  , rerun
  , captureInput
  , releaseInput
  )
where

import Prelude

import KMonad.Keyboard
import KMonad.Util

import RIO.Seq (Seq(..), (><))
import qualified RIO.Seq as Seq


--------------------------------------------------------------------------------
-- $env
--
--


data Dispatch = Dispatch
  { _keyChan  :: TChan KeyEvent
  , _rerunBuf :: TVar (Seq KeyEvent)
  , _redirect :: TMVar (KeyEvent -> IO ())
  }
makeClassy ''Dispatch

mkDispatch :: HasLogFunc e => RIO e KeyEvent -> ContT r (RIO e) Dispatch
mkDispatch src = do
  -- Initialize variables
  kch <- lift . atomically $ newTChan
  rrb <- lift . atomically $ newTVar Seq.empty
  rdr <- lift . atomically $ newEmptyTMVar

  -- Launch thread that copies keys from OS into Chan
  launch_ "dispatch:keyIO-copy" $
    src >>= \e -> atomically (writeTChan kch e)

  -- Return the initialized 'Dispatch' object
  pure $ Dispatch kch rrb rdr

-- | Try to read a KeyEvent.
--
-- If there are any events on the 'rerunBuf', those are consumed head first.
-- Afterwards, we try to pull events from the TChan of raw OS input. If no
-- capturing process is registered, this function returns a 'Left KeyEvent',
-- otherwise it returns a 'Right IO ()'. This IO action needs to be executed to
-- dispatch the 'KeyEvent' to the capturing process.
step :: HasDispatch e => RIO e (Either KeyEvent (IO ()))
step = view dispatch >>= \d -> atomically $ do

  let pullRR = readTVar (d^.rerunBuf) >>= \case
        Seq.Empty -> retrySTM
        (e :<| b) -> do
          writeTVar (d^.rerunBuf) b
          pure e

  e <- pullRR `orElse` readTChan (d^.keyChan)

  tryTakeTMVar (d^.redirect) >>= \case
    Nothing -> pure . Left  $ e
    Just f  -> pure . Right $ f e

-- | Keep trying to read a KeyEvent until it succeeds.
pull :: HasDispatch e => RIO e KeyEvent
pull = step >>= \case
  Left e   -> pure e
  Right io -> liftIO io >> pull

-- | Add a list of 'KeyEvent's to the front of the rerun-buffer. These events
-- will be taken before any new events are read from the OS. The list is
-- consumed head-first.
rerun :: HasDispatch e => [KeyEvent] -> RIO e ()
rerun es = do
  rr <- view rerunBuf
  atomically $ modifyTVar rr (Seq.fromList es ><)

-- | Register a capturing process with the 'Dispatch'
--
-- NOTE: This blocks if the input is already captured by a different process.
--
captureInput :: HasDispatch e => (KeyEvent -> RIO e ()) -> RIO e ()
captureInput f = do
  u   <- askRunInIO
  rdr <- view redirect
  atomically $ putTMVar rdr (u . f)

-- | Remove the registered capture from the 'Dispatch'
--
-- NOTE: This blocks if there is no registered process capturing the input.
--
releaseInput :: HasDispatch e => RIO e ()
releaseInput = view redirect >>= \rdr -> void . atomically $ takeTMVar rdr
