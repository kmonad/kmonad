{-# LANGUAGE DeriveAnyClass #-}
{-|
Module      : KMonad.Api.App
Description : The entry-point for the IO-api
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

-}
module KMonad.Api.App
  ( startAppIO
  )
where

import Control.Concurrent (threadDelay, forkIO)
import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.Logger
import Control.Monad.Reader

import KMonad.Core
import KMonad.Domain.Button
import KMonad.Domain.Effect
import KMonad.Domain.Loop
import KMonad.Api.Encode
import KMonad.Api.EventTracker
import KMonad.Api.InputDispatch
import KMonad.Api.KeyIO
import KMonad.Api.LayerStack
import KMonad.Api.LockManager
import KMonad.Api.Sluice
import KMonad.Api.SymbolEncoder

import qualified UnliftIO.Async as A

--------------------------------------------------------------------------------

-- | The different kinds of errors that can be thrown by App
data AppError
  = AppConfigError ConfigError
  | AppKeyIOError  KeyIOError
  deriving (Show, Exception)
makeClassyPrisms ''AppError

-- | Hooking ConfigError into AppError
instance AsConfigError AppError where
  _ConfigError = _AppError . _ConfigError


--------------------------------------------------------------------------------

-- | Define the App Monad in which we will run our IO Api
newtype App a = App
  { unApp :: LoggingT (ReaderT AppEnv (ExceptT AppError IO)) a
  } deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadLogger
    , MonadReader AppEnv
    , MonadIO
    , MonadError AppError
    )

-- | Deal with Buttons using the IO Monad
instance MonadButton App where
  runButton b x = join $ runButtonIO b x

-- | Emit keys by sending them to the OS through the emitter
instance MonadEmit App where
  emitKey e = view emitter >>= \f -> liftIO $ f =<< (withNow $ actAtTime e)

-- | Deal with fork requests by using forkIO
instance MonadFork App where
  fork a = do
    env <- ask
    void . liftIO $ forkIO (runAppIO a env)

-- | Deal with future requests by pinning the current event to a channel
instance MonadFuture App where
  waitNext = view eventTracker >>= pin

-- | Handle events using the handleApp function
instance MonadHandler App where
  handle = handleApp

-- | Deal with Hold manipulation by changing the Sluice state
instance MonadHold App where
  hold b = view sluice >>= \l -> if b then closeSluice l else openSluice l

-- | Inject events by writing them to the eventSource's injectV
instance MonadInject App where
  injectEvent e = writeEvent e =<< view inputDispatch

-- | Handle lock events using the LockManager
instance MonadLock App where
  lockOn lk = do
    $(logInfo) $"Engaging: " <> tshow lk
    lmLockOn lk
  lockOff lk = do
    $(logInfo) $ "Releasing: " <> tshow lk
    lmLockOff lk
  lockToggle lk = do
    $(logInfo) $ "Toggling: " <> tshow lk
    lmLockToggle lk

-- | Deal with masking input through the EventTracker object
instance MonadMaskInput App where
  maskInput = maskEvent =<< view eventTracker

-- | Await new events by polling the eventSource
instance MonadNext App where
  nextEvent = readEvent =<< view inputDispatch

-- | Get the time from the OS
instance MonadNow App where
  now = liftIO nowIO

-- | Race items using the Async library
instance MonadRace App where
  race a b = ask >>= \env -> liftIO $ A.race (runAppIO a env) (runAppIO b env)

-- | Handle StackManip by operating on the LayerStack
instance MonadStackManip App where
  pushL lid = do
    $(logInfo) $ "Pushing layer: " <> lid
    view layerStack >>= pushLS lid
  popL  lid = do
    $(logInfo) $ "Popping layer: " <> lid
    view layerStack >>= popLS  lid

-- | Dead with special symbols by looking them up using the encoder ring
instance MonadSymbol App where
  emitSymbol ss = encodeSymbol ss >>= \case
    Just ks -> emitSeq ks
    Nothing -> pure ()
  emitDeadKey dk = encodeDeadKey dk >>= \case
    Just ks -> emitSeq ks
    Nothing -> pure ()

-- | Deal with var-requests simply through IO
instance MonadVar App where
  getVar = getV
  putVar = putV

-- | Deal with wait-requests by using threadDelay
instance MonadWait App where
  wait ms = liftIO . threadDelay . fromIntegral $ ms

-- | Run an App action and deal with errors using Either
runApp :: App a -> AppEnv -> IO (Either AppError a)
runApp m env = runExceptT $ runReaderT (runLog $ unApp m) env
  where
    runLog    = runStdoutLoggingT . filterLogger f
    f _ lvl = lvl /= LevelDebug

-- | Run an App action and deal with errors by throwing Exceptions
runAppIO :: App a -> AppEnv -> IO a
runAppIO m env = runApp m env >>= \case
  Left e  -> throwIO e
  Right a -> return a


--------------------------------------------------------------------------------

-- | Handle a KeyEvent by
-- 1) updating the event-tracker
-- 2) construct the action of handling with the layerstack
-- 3) pass that action into the sluice
--
-- Note that, if we are holding, the eventTracker will *still* be updated, but
-- the layerStack handling will not run.
handleApp :: KeyEvent -> App ()
handleApp ke = do
  -- Broadcast event and decide whether to handle
  $(logDebug) "Running handler"
  b <- update ke =<< view eventTracker

  -- When handling, feed the action of handling the key-event into the sluice
  when b $ do
    sl <- view sluice
    lt <- view layerStack
    feed (handleWith ke lt) sl


--------------------------------------------------------------------------------

-- | The AppCfg environment that contains everything required to start the App
-- monad API.
data AppCfg = AppCfg
  { _cfgKeySource   :: KeySource          -- ^ The 'KeySource' interface to the OS input keyboard
  , _cfgKeySink     :: KeySink            -- ^ The 'KeySink' interface to the OS simulated output keyboard
  , _cfgRestart     :: Maybe Microseconds -- ^ If, and how long, to wait before attempting restart
  , _cfgLayerStack  :: LayerMap           -- ^ The nested alist of Layer, Keycode, ButtonToken correspondences
  , _cfgEntry       :: Name            -- ^ The first layer we start in
  }

-- | Interpret a 'Config' as a valid 'AppCfg'
--
-- TODO: make '_cfgRestart' a passed value.
interpretConfig :: Maybe Microseconds -> Config -> AppCfg
interpretConfig us cfg = AppCfg
  { _cfgKeySource  = pickInputIO $ cfg^.input
  , _cfgKeySink    = pickOutputIO $ cfg^.output
  , _cfgRestart    = us
  , _cfgLayerStack = cfg^.mappings
  , _cfgEntry      = cfg^.entry
  }

-- | The AppEnv reader environment that all App actions have access to.
data AppEnv = AppEnv
  { _appEmitter       :: KeyEvent -> IO () -- ^ An action that emits keys to the OS
  , _appInputDispatch :: InputDispatch     -- ^ The entrypoint for events
  , _appEventTracker  :: EventTracker      -- ^ Tracking and masking events
  , _appSluice        :: Sluice App ()     -- ^ Sluice to enable holding processing
  , _appLayerStack    :: LayerStack App    -- ^ LayerStack to manage the layers
  , _appLockManager   :: LockManager       -- ^ State to keep track of lock-style keys
  , _appSymbolEncoder :: SymbolEncoderRing -- ^ State to keep track of Symbol encodings
  }

makeClassy ''AppCfg
makeClassy ''AppEnv

-- | Various ClassyLenses plugging in
instance HasInputDispatch     AppEnv          where inputDispatch     = appInputDispatch
instance HasEmitter           AppEnv          where emitter           = appEmitter
instance HasEventTracker      AppEnv          where eventTracker      = appEventTracker
instance HasSluice            AppEnv App ()   where sluice            = appSluice
instance HasLayerStack        AppEnv App      where layerStack        = appLayerStack
instance HasLockManager       AppEnv          where lockManager       = appLockManager
instance HasSymbolEncoderRing AppEnv          where symbolEncoderRing = appSymbolEncoder


--------------------------------------------------------------------------------

-- | Run KMonad until an error occurs
runOnce :: AppCfg -> IO ()
runOnce cfg = do
  -- Acquire the requested IO resources
  withKeySource (cfg^.cfgKeySource) $ \src ->
    withKeySink (cfg^.cfgKeySink) $ \snk -> do

      -- Initialize the AppEnv variable
      idsp <- mkInputDispatch src
      etrc <- mkEventTracker
      slce <- mkSluice
      stck <- mkLayerStack (cfg^.cfgLayerStack) (cfg^.cfgEntry)
      lmgr <- mkLockManager

      -- Make this customizable at some point
      encd <- mkSymbolEncoderRing [("compose", composeSymbol)]
                                  [("compose", composeDeadKey)]

      let env = AppEnv
            { _appEmitter       = snk
            , _appInputDispatch = idsp
            , _appEventTracker  = etrc
            , _appSluice        = slce
            , _appLayerStack    = stck
            , _appLockManager   = lmgr
            , _appSymbolEncoder = encd
            }

      -- Start the event loop
      runApp loop env >>= \case
        Left e   -> throwIO e
        Right () -> pure ()

-- | Take a Config and start running the event loop, if an error occurs, handle
-- it by either restarting, or displaying the error.
-- FIXME - don't default delay, include in commandline interface at some point
startApp :: AppCfg -> IO ()
startApp cfg = catch (runOnce cfg) $ handleAppError cfg

-- | Decide what to do with an error based on the error and the configuration
handleAppError :: AppCfg -> IOException -> IO ()
handleAppError cfg e = do
  case cfg^.cfgRestart of
    Nothing -> print e
    Just us -> do
      threadDelay $ fromIntegral us
      print e
      putStrLn "Encountered raw IO Exception attempting restart"
      startApp cfg


-- | Take a Config and start running the event loop, if an error occurs, print
-- it and exit.
startAppIO :: Config -> IO ()
startAppIO = startApp . interpretConfig (Just 1000000)

  -- startApp cfg >>= \case
  -- Left e  -> print e
  -- Right _ -> return ()


--------------------------------------------------------------------------------



