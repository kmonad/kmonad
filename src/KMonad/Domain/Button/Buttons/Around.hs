{-|
Module      : KMonad.Domain.Button.Buttons.Around
Description : A button that nests two other buttons
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

-}
module KMonad.Domain.Button.Buttons.Around
  ( mkAround
  , mkAroundM
  )
where


import KMonad.Core


-- | Return a button that manages two other buttons, A and B, and turns a Press
-- into PressA >> PressB and a Release into ReleaseB >> ReleaseA.
mkAround :: Monad m
  => Button m -- ^ The button on the /outside/
  -> Button m -- ^ The button on the /inside/
  -> Button m -- ^ The resulting button
mkAround a b = mkButton $ \case
  Engaged   -> press a   >> press b
  Disengaged -> release b >> release a

-- | Return a button that manages two other buttons, A and B, and turns a Press
-- into PressA >> PressB and a Release into ReleaseB >> ReleaseA. Return it from
-- any arbitrary Monad.
mkAroundM :: (Monad m, Monad n)
  => Button m     -- ^ The button on the /outside/
  -> Button m     -- ^ The button on the /inside/
  -> n (Button m) -- ^ The resulting button
mkAroundM a = return . mkAround a
