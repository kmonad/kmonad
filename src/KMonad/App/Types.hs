module KMonad.App.Types
  ( AppCfg(..)
  , AppEnv(..)
  , HasAppCfg(..)
  , HasAppEnv(..)
  , App
  )
where

import KMonad.Prelude

import KMonad.App.KeyIO
import KMonad.Model.Types
import KMonad.Util

--------------------------------------------------------------------------------
-- $appcfg
--
-- The 'AppCfg' and 'AppEnv' records store the configuration and runtime
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
data AppCfg = AppCfg
  { _keyInputCfg  :: KeyInputCfg  -- ^ The configuration of the input keyboard
  , _keyOutputCfg :: KeyOutputCfg -- ^ The configuration of the output keyboard
  , _acModelCfg   :: ModelCfg     -- ^ The keymap/model configuration
  , _allowCmd     :: Bool         -- ^ Whether shell-commands are allowed
  , _startDelay   :: Ms           -- ^ How long to wait before acquiring the input keyboard
  }
makeClassy ''AppCfg

instance HasModelCfg AppCfg where modelCfg = acModelCfg

-- | Environment of a running KMonad app-loop
data AppEnv = AppEnv
  { -- Stored copy of cfg
    _keAppCfg   :: AppCfg

    -- General IO
  , _keLogEnv   :: LogEnv
  , _keySource  :: GetKey
  , _keySink    :: PutKey

    -- API to the model
  , _aeModelAPI :: ModelAPI
  }
makeClassy ''AppEnv

instance HasLogEnv AppEnv where logEnv = keLogEnv
instance HasLogCfg AppEnv where logCfg = logEnv.logCfg
instance HasAppCfg AppEnv where appCfg = keAppCfg
instance HasModelAPI AppEnv where modelAPI = aeModelAPI

type App a = RIO AppEnv a
