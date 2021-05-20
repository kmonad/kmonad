module KMonad.Model.IO

where

import KMonad.Prelude
import KMonad.Util.Logging
import KMonad.Model.Types

withModel :: LUIO m => ModelCfg -> Ctx r m ModelInterface
withModel cfg = mkCtx $ \f -> undefined
