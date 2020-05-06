{-# LANGUAGE TypeFamilies #-}
module KMonad.Testing

where

import KPrelude

import Data.Kind (Type)



data Foo m = Foo (m Int)
data Bar m = Bar (m String)

data Env m = Env
  { _envFoo :: Foo m
  , _envBar :: Bar m
  }
makeLenses ''Env

data OthEnv m = OthEnv
  { _oEnv :: Env m }
makeLenses ''OthEnv

class HasFoo env where
  type FooF env :: Type -> Type
  foo :: Lens' env (Foo (FooF env))

instance HasFoo (Env m) where
  type FooF (Env m) = m
  foo = envFoo

instance HasFoo (OthEnv m) where
  type FooF (OthEnv m) = m
  foo = oEnv . foo
