{-# OPTIONS_GHC -Wno-orphans #-}
module KMonad.Pullchain.Env
  ( ModelEnv(..)
  , HasModelEnv(..)
  , K
  , KEnv(..)
  , module KMonad.Pullchain.Types
  )
where

import KMonad.Prelude
import KMonad.Util

import KMonad.Model.Types
import KMonad.Model.IO

import KMonad.Pullchain.Components.Dispatch as Dp
import KMonad.Pullchain.Components.Hooks    as Hs
import KMonad.Pullchain.Components.Keymap   as Km
import KMonad.Pullchain.Components.Sluice   as Sl

import KMonad.Pullchain.Types

import UnliftIO.Process (CreateProcess(close_fds), createProcess_, shell)

{- NOTE:

So, we have a Pullchain.Types, but the components need to be able to import
that, therefore we put the environment in a separate module. Not ideal, but the
easiest refactor for now. I'll clean this up later.

-}

--------------------------------------------------------------------------------
-- $env
--
-- The runtime environment for the Pullchain model

data ModelEnv = ModelEnv
  { -- Pull chain components
    _dispatch   :: Dp.Dispatch -- ^ Input dispatch
  , _inHooks    :: Hs.Hooks    -- ^ Input hooks
  , _sluice     :: Sl.Sluice   -- ^ Sluice for pausing
  , _keymap     :: Km.MapStack -- ^ Keymap with state

  , _meModelAPI :: ModelAPI    -- ^ API to App
  , _meLogEnv   :: LogEnv      -- ^ Logging environment
  , _meModelCfg :: ModelCfg    -- ^ The config we were provided
  }
makeClassy ''ModelEnv

type K a = RIO ModelEnv a

instance HasModelAPI ModelEnv where modelAPI = meModelAPI
instance HasLogEnv   ModelEnv where logEnv   = meLogEnv
instance HasLogCfg   ModelEnv where logCfg   = logEnv.logCfg
instance HasModelCfg ModelEnv where modelCfg = meModelCfg

--------------------------------------------------------------------------------

instance
  (HasModelEnv e, HasModelAPI e, HasLogEnv e, HasLogCfg e, HasModelCfg e)
  => MonadKIO (RIO e) where
  -- Emitting by sending to the App
  emit e = sendToShell =<< keyEventNow e

  -- Pausing is a simple IO action
  pause = threadDelay . (*1000) . fromIntegral

  -- Holding and rerunning through the sluice and dispatch
  hold b = do
    sl <- view sluice
    di <- view dispatch
    if b then Sl.block sl else Sl.unblock sl >>= Dp.rerun di

  -- Hooking is performed with the hooks component
  register h = view inHooks >>= \hs -> Hs.register hs h

  -- Layer-ops are sent to the 'Keymap'
  layerOp o = view keymap >>= \hl -> Km.layerOp hl o

  -- Injecting by adding to Dispatch's rerun buffer
  inject e = do
    di <- view dispatch
    logDebug $ "Injecting event: " <> dsp e
    Dp.rerun di [e]

  -- Shell-command through spawnCommand
  shellCmd t = do
    f <- view mAllowCmd
    if f then do
      logInfo $ "Running command: " <> dsp t
      spawnCommand . unpack $ t
    else
      logInfo $ "Received but not running: " <> dsp t
   where
    spawnCommand :: MonadIO m => String -> m ()
    spawnCommand cmd = void $ createProcess_ "spawnCommand"
      (shell cmd){ -- We don't want the child process to inherit things like
                   -- our keyboard grab (this would, for example, make it
                   -- impossible for a command to restart kmonad).
                   close_fds   = True
                 }

--------------------------------------------------------------------------------

-- | The complete environment capable of satisfying 'MonadK'
data KEnv = KEnv
  { _kModelEnv :: ModelEnv -- ^ The model environment containing all the components
  , _kBEnv     :: BEnv     -- ^ The environment describing the currently active button
  }
makeClassy ''KEnv

-- NOTE: This is a bit ridiculous, and could be much cleaner
instance HasModelEnv KEnv where modelEnv = kModelEnv
instance HasModelCfg KEnv where modelCfg = modelEnv.modelCfg
instance HasBEnv     KEnv where bEnv     = kBEnv
instance HasLogEnv   KEnv where logEnv   = modelEnv.logEnv
instance HasLogCfg   KEnv where logCfg   = logEnv.logCfg
instance HasModelAPI KEnv where modelAPI = modelEnv.modelAPI

-- | Hook up all the components to the different 'MonadK' functionalities
instance MonadK (RIO KEnv) where
  -- Binding is found in the stored 'BEnv'
  myBinding = view (bEnv.binding)
