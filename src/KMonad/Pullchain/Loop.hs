module KMonad.Pullchain.Loop

where

import KMonad.Prelude
import KMonad.Model
import KMonad.Util

import KMonad.Pullchain.Action
import KMonad.Pullchain.Button
import KMonad.Pullchain.Env

import KMonad.Pullchain.Components.Dispatch as Dp
import KMonad.Pullchain.Components.Hooks    as Hs
import KMonad.Pullchain.Components.Keymap   as Km
import KMonad.Pullchain.Components.Sluice   as Sl

-- | Get the next keyswitch that requires action.
--
-- NOTE: we pull from output of sluice, which means that the event has already
-- gone through dispach, hooks, and sluice. All releases are handled via hooks,
-- so any release that arrives here can safely be ignored.
next :: K KeySwitch
next = view sluice >>= Sl.pull >>= \case
  e | isPress e -> pure e
    | otherwise -> next

-- | Trigger the press for a given keycode
handlePress :: HasCode a => a -> K ()
handlePress c = view keymap >>= flip Km.lookupKey (c^.code) >>= \case
  Nothing -> view fallThrough >>= \b -> when b $ justEmit (c^.code)
  Just b  -> runBEnv b Press >>= \case
    Nothing -> pure () -- If button was already pressed
    Just a  -> do
      env <- view modelEnv
      runRIO (KEnv env b) $ do
        runAction a
        awaitMy Release $ do
          runBEnv b Release >>= maybe (pure ()) runAction
          pure Catch

-- | Simply trigger the emission of a Keycode without making a button. Line up
-- its release via the hooks as well.
justEmit :: Keycode -> K ()
justEmit c = do
  emit (mkPress c)
  await (isReleaseOf c) $ const $ emit (mkRelease c) >> pure Catch

-- | One step of the Pullchain model
step :: K ()
step = next >>= handlePress

