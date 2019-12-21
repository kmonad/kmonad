
module KMonad.Types.Name
  ( HasName(..)
  , Name
  )
where

import KMonad.Prelude

type Name = Text

class HasName a where
  name :: Lens' a Name
