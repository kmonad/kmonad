{- | TODO: insert header

The 'K.Shell.Cfg' submodule contains all of the abstractions that deal with
specifying and applying modifications to KMonad's behavior. Dependency-wise,
this should be the second-most primal submodule of 'K.Shell'. I.e., it may
depend on 'K.Shell.Initial', but on nothing else.

There was the option of leaving functionality-specific configuration as part of
that functionality's module (e.g., 'LogCfg' in the 'K.Shell.Logging' submodule).
The decision to centralize all configuration was deliberate: having it all part
of 1 file helps me maintain overview.

-}
module K.Shell.Cfg
  ( -- * Top level Cfg API
    withCfg

    -- * Reexports
  , module X
  ) where

import K.Shell.Cfg.Initial as X
import K.Shell.Cfg.Cfgable as X
import K.Shell.Cfg.CfgFile as X
import K.Shell.Cfg.Default as X
import K.Shell.Cfg.Expr    as X
import K.Shell.Cfg.Invoc   as X

withCfg :: MonadUnliftIO m => Invoc -> m ShellCfg
withCfg ivk = do
  cfg <- loadCfgFile $ defShellCfg ^. changed ivk . cfgPath
  pure $ defShellCfg ^. changed cfg . changed ivk
