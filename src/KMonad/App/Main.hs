{-# LANGUAGE CPP #-}
{-|
Module      : KMonad.App.Main
Description : The entry-point to KMonad
Copyright   : (c) David Janssen, 2021
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable

-}
module KMonad.App.Main
  ( -- * The entry-point to KMonad
    main
  )
where

import KMonad.Prelude

import KMonad.Args
import KMonad.App.Types
import KMonad.Keyboard
import KMonad.Util
import KMonad.Model



import qualified KMonad.Model.Dispatch as Dp
import qualified KMonad.Model.Hooks    as Hs
import qualified KMonad.Model.Sluice   as Sl
import qualified KMonad.Model.Keymap   as Km

-- FIXME: This should live somewhere else

#ifdef linux_HOST_OS
import System.Posix.Signals (Handler(Ignore), installHandler, sigCHLD)
#endif

--------------------------------------------------------------------------------
-- $start
--
-- How to start KMonad

-- | The first command in KMonad
--
-- Get the invocation from the command-line, then do something with it.
main :: IO ()
main = getCmd >>= runCmd

-- | Execute the provided 'Cmd'
--
-- 1. Construct the log-func
-- 2. Parse the config-file
-- 3. Maybe start KMonad
runCmd :: Cmd -> IO ()
runCmd c = do
  o <- logOptionsHandle stdout False <&> setLogMinLevel (c^.logLvl)
  withLogFunc o $ \f -> runRIO f $ do
    cfg <- loadConfig c
    unless (c^.dryRun) $ startApp cfg

--------------------------------------------------------------------------------
-- $init
--
-- The steps required to turn a configuration into the initial KMonad env

-- | Initialize all the components of the KMonad app-loop
--
-- NOTE: This is written in 'ContT' over our normal RIO monad. This is just to
-- to simplify a bunch of nesting of calls. At no point do we make use of
-- 'callCC' or other 'ContT' functionality.
--
initAppEnv :: HasLogFunc e => AppCfg -> ContT r (RIO e) AppEnv
initAppEnv cfg = do
  -- Get a reference to the logging function
  lgf <- view logFuncL

  -- Wait a bit for the user to release the 'Return' key with which they started KMonad
  threadDelay $ fromIntegral (cfg^.startDelay) * 1000

  -- Acquire the keysource and keysink
  snk <- using $ cfg^.keySinkDev
  src <- using $ cfg^.keySourceDev

  -- Initialize the pull-chain components
  dsp <- Dp.mkDispatch $ awaitKey src
  ihk <- Hs.mkHooks    $ Dp.pull  dsp
  slc <- Sl.mkSluice   $ Hs.pull  ihk

  -- Initialize the button environments in the keymap
  phl <- Km.mkKeymap (cfg^.firstLayer) (cfg^.keymapCfg)

  -- Initialize output components
  otv <- lift newEmptyTMVarIO
  ohk <- Hs.mkHooks . atomically . takeTMVar $ otv

  -- Setup thread to read from outHooks and emit to keysink
  launch_ "emitter_proc" $ do
    e <- atomically . takeTMVar $ otv
    emitKey snk e
  -- emit e = view keySink >>= flip emitKey e
  pure $ AppEnv
    { _keAppCfg  = cfg
    , _keLogFunc = lgf
    , _keySink   = snk
    , _keySource = src

    , _dispatch  = dsp
    , _inHooks   = ihk
    , _sluice    = slc

    , _keymap    = phl
    , _outHooks  = ohk
    , _outVar    = otv
    }

--------------------------------------------------------------------------------
-- $emit
--
-- How to use a KMonad env to emit keys to the OS
--
-- FIXME: this needs to live somewhere else

-- | Trigger the button-action press currently registered to 'Keycode'
pressKey :: (HasAppEnv e, HasLogFunc e, HasAppCfg e) => Keycode -> RIO e ()
pressKey c =
  view keymap >>= flip Km.lookupKey c >>= \case

    -- If the keycode does not occur in our keymap
    Nothing -> do
      ft <- view fallThrough
      when ft $ do
          emit $ mkPress c
          await (isReleaseOf c) $ \_ -> do
            emit $ mkRelease c
            pure Catch

    -- If the keycode does occur in our keymap
    Just b  -> runBEnv b Press >>= \case
      Nothing -> pure ()  -- If the previous action on this key was *not* a release
      Just a  -> do
        -- Execute the press and register the release
        app <- view appEnv
        runRIO (KEnv app b) $ do
          runAction a
          awaitMy Release $ do
            runBEnv b Release >>= \case
              Nothing -> pure ()
              Just a  -> runAction a
            pure Catch

--------------------------------------------------------------------------------
-- $loop
--
-- The app-loop of KMonad

-- | Perform 1 step of KMonad's app loop
--
-- We forever:
-- 1. Pull from the pull-chain until an unhandled event reaches us.
-- 2. If that event is a 'Press' we use our keymap to trigger an action.
loop :: RIO AppEnv ()
loop = forever $ view sluice >>= Sl.pull >>= \case
  e | e^.switch == Press -> pressKey $ e^.keycode
  _                      -> pure ()

-- | Run KMonad using the provided configuration
startApp :: HasLogFunc e => AppCfg -> RIO e ()
startApp c = do
#ifdef linux_HOST_OS
  -- Ignore SIGCHLD to avoid zombie processes.
  liftIO . void $ installHandler sigCHLD Ignore Nothing
#endif
  runContT (initAppEnv c) (`runRIO` loop)
