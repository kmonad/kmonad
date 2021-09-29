module KMonad.Pullchain.IO

where

import KMonad.Prelude
import KMonad.Util hiding (around)
import KMonad.App.Types
import KMonad.Model

import KMonad.Pullchain.Button
import KMonad.Pullchain.Env
import KMonad.Pullchain.Loop (step)
import KMonad.Pullchain.Operations

import qualified KMonad.Pullchain.Components.Dispatch as Dp
import qualified KMonad.Pullchain.Components.Hooks    as Hs
import qualified KMonad.Pullchain.Components.Sluice   as Sl
import qualified KMonad.Pullchain.Components.Keymap   as Km

-- | Use the 'ModelCfg' to start the pullchain and provide an API token
withModel :: LUIO m env => ModelCfg -> Ctx r m ModelAPI
withModel cfg = do
  -- Create the model environment
  env <- initModelEnv cfg

  -- Start the thread that runs the model
  launch_ (runRIO env step)

  -- Run the continuation on the API
  mkCtx $ \f -> f (env^.modelAPI)

-- | Initialize all the components required to run a pullchain model
initModelEnv :: LUIO m env => ModelCfg -> Ctx r m ModelEnv
initModelEnv cfg = do
  -- Fetch the logging env from the caller
  le <- lift $ view logEnv

  -- Create a new ModelAPI
  api <- lift mkModelAPI
  let nxt = view keySwitch <$> recvFromShell

  -- Initialize all the individual buttons
  let bvs = initKeymap (cfg^.keymapCfg)

  -- Initialize the pull-chain
  dsp <- Dp.mkDispatch $ runRIO api nxt
  ihk <- Hs.mkHooks    $ Dp.pull  dsp
  slc <- Sl.mkSluice   $ Hs.pull  ihk
  phl <- Km.mkKeymap (cfg^.firstLayer) bvs

  pure $ ModelEnv
    { -- Components
      _dispatch   = dsp
    , _inHooks    = ihk
    , _sluice     = slc
    , _keymap     = phl

      -- API
    , _meModelAPI = api
    , _meLogEnv   = le
    , _meModelCfg = cfg
    }

