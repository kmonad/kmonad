module KMonad.Daemon.Dispatch
  ( Dispatch
  , HasDispatch
  , dispatch
  , startDispatch
  , pause
  , inject
  , intercept
  , awaitEvent
  )
where

import Prelude hiding (Empty)

import KMonad.Event
import KMonad.Keyboard
import KMonad.Keyboard.IO
import KMonad.Runner
import KMonad.Util

import RIO.Seq (Seq(..), (><))

import qualified KMonad.Button                      as B
import qualified KMonad.Daemon.Dispatch.CChan       as Ch
import qualified KMonad.Daemon.Dispatch.InjectPoint as Ip
import qualified KMonad.Daemon.Dispatch.Sluice      as Sl
import qualified RIO.Seq                            as Seq


--------------------------------------------------------------------------------
-- $disp
--

-- | The 'Dispatch' type containing all the components for processing
data Dispatch = Dispatch
  { _cchan       :: Ch.CChan KeyEvent
  , _cReg        :: MVar [B.Callback]
  , _rerunBuffer :: MVar (Seq KeyEvent)
  , _sluice      :: Sl.Sluice (KeyEvent)
  , _injectP     :: Ip.InjectPoint Event
  }
makeClassy ''Dispatch

-- | Initialize all the values inside a 'Dispatch'. This just initializes all
-- the variables, but does not start the required threads.
initDispatch :: RIO e Dispatch
initDispatch = do
  cc     <- Ch.mkCChan
  rerunB <- newMVar Seq.empty
  creg   <- newMVar []

  -- Take from rerunBuffer if available, otherwise new event from Key IO
  let pullEvent = modifyMVar rerunB $ \case
        Empty       -> (Empty,) <$> Ch.read cc
        (e :<| rst) -> pure (rst, e)

  -- Execute all callbacks, return whether event was captured or not.
  let runCBs e = modifyMVar creg (B.runCallbacks e)
 
  -- Get an event, if it passes the callbacks, return it, otherwise try again
  let inputTick = pullEvent >>= \e -> runCBs e >>= \case
        True  -> inputTick
        False -> pure e

  slce   <- Sl.mkSluice inputTick
  ip     <- Ip.mkInjectPoint (KeyIOEvent <$> Sl.read slce)
  pure $ Dispatch cc creg rerunB slce ip

-- | Return a new 'Dispatch' context
startDispatch :: HasRunEnv e
  => KeySource
  -> ContT r (RIO e) Dispatch
startDispatch src = do
  dsp <- lift initDispatch
  launch_ "dispatch:keyio" $
    awaitKeyWith src >>= Ch.write (dsp^.cchan)
  pure dsp

-- | Pause or unpause the stream
--
-- If we pause the stream, we simply put the 'Sluice' into blocked mode, any new
-- events will be stored in the sluice until we unblock it.
--
-- If we unpause the stream, in the same atomic action we unblock the sluice and
-- prepend any stored events to rerunBuffer. The next sluice-read event will
-- then pull from the head of the rerunBuffer again.
pause :: HasDispatch e => Bool -> RIO e ()
pause True  = Sl.block =<< view sluice
pause False = view dispatch >>= \d ->
  modifyMVar_ (d^.rerunBuffer) $ \r -> do
    es <- Sl.unblock $ d^.sluice
    pure $ es >< r


-- | Inject an event into the Dispatch object
inject :: HasDispatch e => Event -> RIO e ()
inject e = view injectP >>= flip Ip.inject e

-- | Register a 'KeyEvent' intercept
intercept :: HasDispatch e
  => B.Callback -> RIO e ()
intercept c = view cReg >>= flip modifyMVar_ (pure . (c:))

-- | Wait for the next Event
awaitEvent :: HasDispatch e => RIO e Event
awaitEvent = view injectP >>= Ip.read
