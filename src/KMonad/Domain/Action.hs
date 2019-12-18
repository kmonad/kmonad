module KMonad.Domain.Action
  -- (
  -- )
where

import KMonad.Prelude

import KMonad.Core
import KMonad.Domain.Types

data ActionEnv = ActionEnv
  { _kcSelf :: Keycode
  }



runAction :: Action -> RIO e ()
runAction = undefined
