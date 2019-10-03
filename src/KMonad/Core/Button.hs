{-|
Module      : KMonad.Core.Button
Description : The pure types and utilities for Button data.
Copyright   : (c) David Janssen, 2019
License     : MIT

Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : non-portable (MPTC with FD, FFI to Linux-only c-code)


'Button's function as event-handlers that exist in a contextual vacuum. That is
to say, they are expressly isolated from knowing what event they are handling,
or the state of any other 'Button's. They only know whether they are being
'BPress'ed or 'BRelease'd.

When being triggered, a 'Button' will provide some sort of effectful action @m
()@. Where @m@ is the 'Monad' on which 'Button' is parametrized. There are a
variety of effects in 'KMonad.Domain.Effect'.

It is assumed, but not enforced, that when a 'Button' is pressed or released
twice in a row, that the second occurence of the same trigger has no effect. If
you extend KMonad by adding your own 'Button's, you must ensure that this
invariant holds.

prop> bPress b *> bPress b = bPress b
prop> bRelease b *> bRelease b = bRelease b

-}
module KMonad.Core.Button
  ( -- * Core types and constructors
    -- $types
    ButtonSignal(..)
  , Button
  , mkButton
  , runButton

    -- * Utilities for working with Buttons
    -- $util
  , asSignal
  , withSignal
  , bPress
  , bRelease
  , bTap
  )
where


import Control.Lens
import KMonad.Core.Keyboard
import KMonad.Core.Types

--------------------------------------------------------------------------------
-- $types

-- | The two signals that are passed to 'Button's.
--
-- @since 0.1.0
data ButtonSignal = BPress | BRelease
  deriving (Eq, Show)

-- | The basic type of a button, parametrized on the type of action it provides.
--
-- @since 0.1.0
newtype Button m = Button { unButton :: ButtonSignal -> m ()}

-- | Create a new 'Button' from an effectful function from 'ButtonSignal'
--
-- @since 0.1.0
mkButton :: (ButtonSignal -> m ()) -> Button m
mkButton = Button

-- | Run a 'Button' with the provided 'ButtonSignal'
--
-- @since 0.1.0
runButton :: Button m -> ButtonSignal -> m ()
runButton = unButton


--------------------------------------------------------------------------------
-- $util

-- | Translate a 'KeyEvent' into 'Maybe' a 'ButtonSignal' by ignoring all
-- 'Repeat' events.
--
-- @since 0.1.0
asSignal :: (HasType a KeyActionType) => a -> Maybe ButtonSignal
asSignal e = case e^._type of
  Repeat  -> Nothing
  Press   -> Just BPress
  Release -> Just BRelease

-- | Context-creating combinator that runs a function only on valid signals.
-- This makes it easy to run 'Button's on 'KeyEvent's.
--
-- For example:
--
-- >>> withSignal keyEvent $ \signal -> runButton button signal
--
-- @since 0.1.0
withSignal :: (Monoid a, Monad m)
  => KeyEvent
  -> (ButtonSignal -> m a)
  -> m a
withSignal e f = maybe (return mempty) f (asSignal e)

-- | Trigger a 'Button' by passing a 'BPress'
--
-- @since 0.1.0
bPress :: Button m -> m ()
bPress = ($ BPress) . runButton

-- | Trigger a 'Button' by passing a 'BRelease'
--
-- @since 0.1.0
bRelease :: Button m -> m ()
bRelease = ($ BRelease) . runButton

-- | Trigger a 'Button' twice, first with a 'BPress', then with a 'BRelease'
--
-- @since 0.1.0
bTap :: Applicative m => Button m -> m ()
bTap b   = bPress b *> bRelease b
