{-|
Module      : KMonad.Pullchain.Action
Description : Collection of basic operations
Copyright   : (c) David Janssen, 2019
License     : MIT
Maintainer  : janssen.dhj@gmail.com
Stability   : experimental
Portability : portable

KMonad is implemented as an engine that is capable of running 'MonadK' actions.
The logic of various different buttons and keyboard operations are expressed in
this 'MonadK'. This module defines the basic types and operations that make up
'MonadK'. The implementation of how KMonad implements 'MonadK' can be found in
the "KMonad.App" module.

NOTE: All of this is a bit muddled, and redoing the way hooks are handled, and
the basic structuring of MonadK and MonadKIO are liable to change soon.

-}
module KMonad.Pullchain.Action
  ( -- $combs
    my
  , matchMy
  , after
  , whenDone
  , await
  , awaitMy
  , tHookF
  , hookF
  , within
  , withinHeld
  )

where

import KMonad.Prelude

-- import KMonad.Keyboard
import KMonad.Util
import KMonad.Util.Keyboard

import KMonad.Pullchain.Types


--------------------------------------------------------------------------------
-- $util

-- | Create a KeySwitch matching pressing or releasing of the current button.
my :: MonadK m => Switch -> m KeySwitch
my s = mkKeySwitch s <$> myBinding

-- | Register a simple hook without a timeout
hookF :: MonadKIO m => (KeySwitch -> m Catch) -> m ()
hookF f = register $ Hook Nothing $ \t -> f (t^.event)

-- | Register a hook with a timeout
tHookF :: MonadK m
  => Ms                   -- ^ The timeout delay for the hook
  -> m ()                 -- ^ The action to perform on timeout
  -> (Trigger -> m Catch) -- ^ The action to perform on trigger
  -> m ()                 -- ^ The resulting action
tHookF d a f = register $ Hook (Just $ Timeout d a) f

-- | Perform an action after a period of time has elapsed
--
-- This is essentially just a way to perform async actions using the KMonad hook
-- system.
after :: MonadK m
  => Ms
  -> m ()
  -> m ()
after d a = do
  let rehook t = after (d - t^.elapsed) a *> pure NoCatch
  tHookF d a rehook

-- | Perform an action immediately after the current action is finished. NOTE:
-- there is no guarantee that another event doesn't outrace this, only that it
-- will happen as soon as the CPU gets to it.
whenDone :: MonadK m
  => m ()
  -> m ()
whenDone = after 0


-- | Create a KeyPred that matches the Press or Release of the current button.
matchMy :: MonadK m => Switch -> m KeyPred
matchMy s = (==) <$> my s

-- | Wait for an event to match a predicate and then execute an action
await :: MonadKIO m => KeyPred -> (KeySwitch -> m Catch) -> m ()
await p a = hookF $ \e -> if p e
  then a e
  else await p a *> pure NoCatch

-- | Execute an action on the detection of the Switch of the active button.
awaitMy :: MonadK m => Switch -> m Catch -> m ()
awaitMy s a = matchMy s >>= flip await (const a)

-- | Try to call a function on a succesful match of a predicate within a certain
-- time period. On a timeout, perform an action.
within :: MonadK m
  => Ms          -- ^ The time within which this filter is active
  -> m KeyPred             -- ^ The predicate used to find a match
  -> m ()                  -- ^ The action to call on timeout
  -> (Trigger -> m Catch)  -- ^ The action to call on a succesful match
  -> m ()                  -- ^ The resulting action
within d p a f = do
  p' <- p
  -- define f' to run action on predicate match, or rehook on predicate mismatch
  let f' t = if p' (t^.event)
        then f t
        else within (d - t^.elapsed) p a f *> pure NoCatch
  tHookF d a f'

-- | Like `within`, but acquires a hold when starting, and releases when done
withinHeld :: MonadK m
  => Ms          -- ^ The time within which this filter is active
  -> m KeyPred             -- ^ The predicate used to find a match
  -> m ()                  -- ^ The action to call on timeout
  -> (Trigger -> m Catch)  -- ^ The action to call on a succesful match
  -> m ()                  -- ^ The resulting action
withinHeld d p a f = do
  hold True
  within d p (a <* hold False) (\x -> f x <* hold False)
