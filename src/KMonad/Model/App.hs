{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE UndecidableInstances #-}
module KMonad.Model.App
  ( AppEnv(..)
  , KEnv(..)
  , HasCfg(..)
  , HasAppEnv(..)
  , HasKEnv(..)
  )
where

import UnliftIO.Process (CreateProcess(close_fds), createProcess_, shell)

import KMonad.Keyboard
import KMonad.Model.Cfg
import KMonad.Model.BEnv
import KMonad.Model.Action

import qualified KMonad.Model.Dispatch as Dp
import qualified KMonad.Model.Hooks    as Hs
import qualified KMonad.Model.Sluice   as Sl
import qualified KMonad.Model.Keymap   as Km

--------------------------------------------------------------------------------
-- $appenv
--
-- The 'ACfg' and 'AppEnv' records store the configuration and runtime
-- environment of KMonad's app-loop respectively. This contains nearly all of
-- the components required to run KMonad.
--
-- Note that the 'AppEnv' is still not sufficient to satisfy 'MonadK', since
-- there are times where we are not processing a button push. I.e. 'MonadK' is a
-- series of operations that allow us to specify how to deal with the current
-- button-push, but it required us to have actually registered a push (or
-- release) of some button. 'AppEnv' exists before any buttons have been pushed,
-- and therefore contains no information about 'the current button push' (nor
-- could it). Later in this module we specify KEnv as a combination of AppEnv
-- and a BEnv. It is that environment that we use to satisfy 'MonadK'.

-- | Record of all the configuration options required to run KMonad's core App
-- loop.


-- | Environment of a running KMonad app-loop
data AppEnv = AppEnv
  { -- Stored copy of cfg
    _ecfg   :: ECfg

    -- Pull chain
  , _dispatch   :: Dp.Dispatch
  , _inHooks    :: Hs.Hooks
  , _sluice     :: Sl.Sluice

    -- Other components
  , _outHooks   :: Hs.Hooks
  , _outVar     :: TMVar KeyEvent
  }
makeClassy ''AppEnv

instance HasLogFunc AppEnv  where logFuncL = ecfg . logging
instance HasCfg AppEnv 'Env where cfg      = ecfg

--------------------------------------------------------------------------------
-- $kenv
--

-- | The complete environment capable of satisfying 'MonadK' for a single key
data KEnv = KEnv
  { _kAppEnv :: AppEnv -- ^ The app environment containing all the components
  , _kBEnv   :: BEnv   -- ^ The environment describing the currently active button
  }
makeClassy ''KEnv

instance HasCfg KEnv 'Env where cfg       = kAppEnv.cfg
instance HasAppEnv   KEnv where appEnv    = kAppEnv
instance HasBEnv     KEnv where bEnv      = kBEnv
instance HasLogFunc  KEnv where logFuncL  = kAppEnv.logFuncL

-- | Hook up all the components to the different 'MonadK' functionalities
instance MonadK (RIO KEnv) where
  -- Binding is found in the stored 'BEnv'
  myBinding = view (bEnv.binding)

instance (HasAppEnv e, HasCfg e 'Env, HasLogFunc e) => MonadKIO (RIO e) where
  -- Emitting with the keysink
  emit e = view outVar >>= atomically . flip putTMVar e
  -- emit e = view keySink >>= flip emitKey e

  -- Pausing is a simple IO action
  pause = threadDelay . (*1000) . fromIntegral

  -- Holding and rerunning through the sluice and dispatch
  hold b = do
    sl <- view sluice
    di <- view dispatch
    if b then Sl.block sl else Sl.unblock sl >>= Dp.rerun di

  -- Hooking is performed with the hooks component
  register l h = do
    hs <- case l of
      InputHook  -> view inHooks
      OutputHook -> view outHooks
    Hs.register hs h

  -- Layer-ops are sent to the 'Keymap'
  layerOp o = view keymap >>= \hl -> Km.layerOp hl o

  -- Injecting by adding to Dispatch's rerun buffer
  inject e = do
    di <- view dispatch
    logDebug $ "Injecting event: " <> display e
    Dp.rerun di [e]

  -- Shell-command through spawnCommand
  shellCmd t = do
    f <- view allowCmd
    if f then do
      logInfo $ "Running command: " <> display t
      spawnCommand . unpack $ t
    else
      logInfo $ "Received but not running: " <> display t
   where
    spawnCommand :: MonadIO m => String -> m ()
    spawnCommand cmd = void $ createProcess_ "spawnCommand"
      (shell cmd){ -- We don't want the child process to inherit things like
                   -- our keyboard grab (this would, for example, make it
                   -- impossible for a command to restart KMonad).
                   close_fds   = True
                 }
