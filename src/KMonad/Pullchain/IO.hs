module KMonad.Pullchain.IO

where

import KMonad.Prelude
import KMonad.Util.Keyboard
import KMonad.App.Types
import qualified KMonad.Pullchain.Components.Keymap   as Km

import KMonad.Pullchain -- FIXME: Once Model and App are correctly separated, this import should not be here anymore.

-- | Trigger the button-action press currently registered to 'Keycode'
--
-- This is the actual dispatch function used to trigger the model to do things.
pressKey :: CanK e => Keycode -> RIO e ()
pressKey c =
  view keymap >>= flip Km.lookupKey c >>= \case

    -- If the keycode does not occur in our keymap
    Nothing -> do
      ft <- view fallThrough
      if ft
        then do
          emit $ mkPress c
          await (isReleaseOf c) $ \_ -> do
            emit $ mkRelease c
            pure Catch
        else pure ()

    -- If the keycode does occur in our keymap
    Just b  -> runBEnv b Press >>= \case
      Nothing -> pure ()  -- If the previous action on this key was *not* a release
      Just a  -> do
        -- Execute the press and register the release
        app <- view appEnv
        runRIO (KEnv app b) $ do
          runAction a
          awaitMy Release $ do
            runBEnv b Release >>= maybe (pure ()) runAction
            pure Catch
