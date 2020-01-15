module KLisp.Token

where

import Prelude

import KMonad



data Expr a where
  LitKA   :: KeyAction           -> Expr KeyAction
  LitBool :: Bool                -> Expr Bool
  LitMS   :: Milliseconds        -> Expr Milliseconds
  WaitFor :: (KeyAction -> Bool) -> Expr KeyAction
  Emit    :: Expr KeyAction      -> Expr ()
  Pause   :: Expr Milliseconds   -> Expr ()
  Hold    :: Expr Bool           -> Expr ()
  Fork    :: Expr a              -> Expr ()
  Race    :: Expr a -> Expr (a -> b) -> Expr c -> Expr (c -> d) -> Expr (Either b d)
