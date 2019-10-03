{-|
Module      : KMonad.Domain.Button.TapNext
Description : A button that behaves differently depending on the next keypress
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)

The TapNext button is another way of specifying a button that behaves
differently when tapped or help (another example being the TapHold). Where a
TapHold is based around a timer, the TapNext only considers the next keypress.
When the next keypress is a release of the same key, we register one key, when
it is something else, we behave as if we are being held.

-}
module KMonad.Domain.Button.TapNext
  ( mkTapNext
  )
where

import Control.Lens
import Control.Monad.Trans

import KMonad.Core
import KMonad.Domain.Effect

data BState
  = Unpressed
  | Pressed
  deriving (Eq, Show)

-- | Return a new TapNext button
mkTapNext :: (CanButton m, MonadIO n)
  => Button m  -- ^ The button to use for taps
  -> Button m  -- ^ The button to use for holds
  -> n (Button m)
mkTapNext t h = runVar Unpressed $ \v -> do
  mkButton $ \x -> go v x t h

go :: CanButton m
  => Var BState   -- ^ Local var of current button state
  -> ButtonSignal -- ^ The signal triggering this button
  -> Button m     -- ^ The button to use for taps
  -> Button m     -- ^ The button to use for holds
  -> m ()
go v x t h = do
  st <- getVar v
  case (st, x) of
    (Unpressed, BPress) -> putVar Pressed   v >> hPress v t h
    (Pressed, BRelease) -> putVar Unpressed v >> bRelease h
    _                   -> putVar st        v

-- | Await the next event before deciding what to do
hPress :: CanButton m
  => Var BState -- ^ Local var of the current button state
  -> Button m   -- ^ The button to use for taps
  -> Button m   -- ^ The button to use for holds
  -> m ()
hPress v t h = do
  hold True
  nxt <- pinComparison
  let f = nxt >>= \cmp -> if
        -- If next event is another press (by definition another button), start hold
        | cmp^._type == Press -> bPress h
        -- If next event is the release of the TapNext button, tap
        | cmp^._type == Release && cmp^.sameCode
          -> bTap t >> swapVar Unpressed v >> return ()
        -- Otherwise loop
        | otherwise               -> f
  fork (f >> hold False)
  
