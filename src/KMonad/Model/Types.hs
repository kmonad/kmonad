module KMonad.Model.Types

where

import KMonad.Prelude
import KMonad.Util

--------------------------------------------------------------------------------
-- $basic
--
-- A collection of `simple` types used throughout the Model


--------------------------------------------------------------------------------
-- $

data ModelCfg = ModelCfg
  { _getKey :: GetKey -- ^ How the model acquires the next 'KeyEvent'
  , _putKey :: PutKey -- ^ How the model emits 'KeyEvent's to the OS
  }

--------------------------------------------------------------------------------

