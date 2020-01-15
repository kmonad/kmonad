{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
module KMonad.Daemon.KeyHandler

where


import Prelude

import KMonad.Button
import KMonad.Keyboard
import KMonad.Util

import qualified Data.LayerStack as Ls


-- | The configuration of a 'Button' with some additional state to keep track of
-- the last 'SwitchAction'
data ButtonEnv = ButtonEnv
  { _beButton   :: !Button              -- ^ The configuration for this button
  , _binding    :: !Keycode             -- ^ The 'Keycode' to which this button is bound
  , _lastAction :: !(MVar SwitchAction) -- ^ State to keep track of last manipulation
  }
makeClassy ''ButtonEnv

-- | Anything that has a ButtonEnv has a Button
instance {-# OVERLAPS #-} (HasButtonEnv e) => HasButton e
  where button = buttonEnv . beButton

-- | Initialize a 'Button' from a 'Button' and a binding
mkButtonEnv :: Button -> Keycode -> RIO e ButtonEnv
mkButtonEnv b c = ButtonEnv b c <$> newMVar Release

-- | Run a 'Button' on a 'SwitchAction' returning an 'Action' to be performed by
-- the engine. This will only do something if a 'Press' is followed a 'Release'
-- or vice-versa, pressing or releasing more than once in sequence does nothing.
runButton :: HasButtonEnv e => SwitchAction -> RIO e (Maybe (Action ()))
runButton a = do
  b <- view buttonEnv
  modifyMVar (b^.lastAction) $ \l -> pure $ case (a, l) of
    (Press, Release) -> (Press,   Just $ b^.pressAction)
    (Release, Press) -> (Release, Just $ b^.releaseAction)
    _                -> (a,       Nothing)


--------------------------------------------------------------------------------
-- $env
--

-- | 'Keymap's are mappings from 'Name'd maps from 'Keycode' to things.
type Keymap a = Ls.LayerStack Name Keycode a

-- |
data KeyHandler = KeyHandler
  { _keymap :: TVar (Keymap ButtonEnv)
  }
makeClassy ''KeyHandler

mkKeyHandler' :: Keymap Button -> RIO e KeyHandler
mkKeyHandler' m = do
  envs <- m & Ls.items . itraversed %%@~ \(_, c) b -> mkButtonEnv b c
  v    <- atomically $ newTVar envs
  pure $ KeyHandler v

mkKeyHandler :: Keymap Button -> ContT r (RIO e) KeyHandler
mkKeyHandler = lift . mkKeyHandler'

--------------------------------------------------------------------------------
-- $keyh

-- | Lookup the 'ButtonEnv' currently mapped to the key press.
--
-- This returns Just 'ButtonEnv' when the input event is a 'Press' and there is
-- a 'ButtonEnv' stored in the 'keymap' at the corresponding 'Keycode'. In any
-- other scenario this returns 'Nothing'.
lookupKey :: HasKeyHandler e
  => KeyAction               -- ^ The 'KeyAction' to handle
  -> RIO e (Maybe ButtonEnv) -- ^ The resulting action
lookupKey a
  | isPress a = do
      km <- atomically . readTVar =<< view keymap
      pure $ km ^? Ls.atKey (a^.keycode)
  | otherwise = pure $ Nothing




--------------------------------------------------------------------------------
-- $manip
--
-- We provide 2 actions to manipulate the the layer-stack. One that adds a layer
-- to the front, and one that pops the first occurence of a layer. Note that
-- both of these actions can throw errors.

-- | Push a layer to the front of the stack. This throws an error if the
-- provided 'Name' does not correspond to any map.
-- pushLayer :: (HasKeymap e, HasLogFunc e) => Name -> RIO e ()
-- pushLayer n =
--   view keymap >>= flip modifyMVar (pure . go) >>= \case
--     Left e  -> do
--       logError $ "Error pushing layer: " <> display n
--       throwIO e
--     Right _ ->
--       logInfo $ "Pushed layer: " <> display n
--   where go st = case Ls.pushLayer n st of
--           Left e   -> (st, Left e)
--           Right km -> (km, Right ())

-- -- | Pop a layer from the stack. This throws a 'LayerNotOnStackError' if the
-- -- layer does not exist on the stack, or a 'LayerNotFoundError' if the layer
-- -- does not exist in the keymap at all.
-- popLayer :: (HasKeymap e, HasLogFunc e) => Name -> RIO e ()
-- popLayer n =
--   view keymap >>= flip modifyMVar (pure . go) >>= \case
--     Left e  -> do
--       logError $ "Error popping layer: " <> display n
--       throwIO e
--     Right _ ->
--       logInfo $ "Popped layer: " <> display n
--   where go st = case Ls.popLayer n st of
--           Left e   -> (st, Left e)
--           Right km -> (km, Right ())
