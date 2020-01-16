module KLisp.Token

where

import KPrelude

import KMonad

data KVal
  = Atom         Text
  | List         [KVal]
  | Number       Integer
  | Text         Text
  | Bool         Bool
  | Keycode      Keycode
  | SwitchAction SwitchAction

data Expr :: * -> * where
  -- MonadButton actions
  Emit       :: Expr KeyAction -> Expr ()
  Pause      :: Expr Milliseconds -> Expr ()
  Hold       :: Expr Bool -> Expr ()
  HookNext   :: Expr HookPred -> Expr (HookFun Expr) -> Expr ()
  HookWithin :: Expr Milliseconds -> Expr HookPred -> Expr (HookFun Expr) -> Expr ()
  MyBinding  :: Expr Keycode



  -- Simply typed lambda calculus
  Lift :: a -> Expr a
  Lam :: (Expr a -> Expr b) -> Expr (a -> Expr b)
  App :: Expr (a -> Expr b) -> Expr a -> Expr b
  Fix :: Expr (a -> Expr a) -> Expr a

eval :: MonadButton m => Expr a -> m a
eval (Emit ka) = emit =<< eval ka
eval (Pause ms) = pause =<< eval ms
eval (Hold b) = hold =<< eval b
eval (HookNext p f) = do
  p' <- eval p
  f' <- eval f
  hookNext p' (eval . f')
eval (HookWithin ms p f) = do
  ms' <- eval ms
  p' <- eval p
  f' <- eval f
  hookWithin ms' p' (eval . f')
eval MyBinding = myBinding
eval (Lift a) = pure a
eval (App f x) = do
  f' <- eval f
  x' <- eval x
  eval $ f' x'
eval (Lam f) = pure $ \x -> (f (Lift x))
eval (Fix f) = do
  f' <- eval f
  x' <- eval (Fix f)
  eval $ f' x'

  -- eval f >>= (<$> eval (Fix f))



-- foo :: (Maybe a -> Maybe b) -> Maybe (a -> b)
-- foo =
