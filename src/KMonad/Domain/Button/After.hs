{-|
Module      : KMonad.Domain.Button.After
Description : A button that performs 2 button taps in a row
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Domain.Button.After
  ( mkAfter
  , mkAfterM
  )
where


import KMonad.Core.Button


-- | Return a button that manages two other buttons, A and B, and turns a Press
-- into TapA >> TapB and does nothing on release
mkAfter :: Monad m
  => Button m -- ^ The button to run first
  -> Button m -- ^ The button to run second
  -> Button m -- ^ The resulting button
mkAfter a b = mkButton $ \case
  BPress   -> bTap a >> bTap b
  BRelease -> pure ()

-- | Like mkAfter, but return from any arbitrary Monad
mkAfterM :: (Monad m, Monad n)
  => Button m     -- ^ The button to run first
  -> Button m     -- ^ The button to run second
  -> n (Button m) -- ^ The resulting button
mkAfterM a = return . mkAfter a
