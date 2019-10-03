{-|
Module      : KMonad.Core.Types
Description : The most core of type-aliases and classes
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Core.Types

where

import Control.Lens
import Data.Text (Text)


-- | For things that have a name

type Name = Text

class HasName a where
  name :: a -> Name


-- | For things that have a type

class HasType a b | a -> b where
  _type :: Lens' a b
