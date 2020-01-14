module KMonad.Button.Keymap

where

import Prelude

import KMonad.Button.Types
import KMonad.Keyboard
import KMonad.Util

import qualified Data.LayerStack as Ls

--------------------------------------------------------------------------------

-- | 'Keymap's are mappings 'Name'd maps from 'Keycode' to things.
type Keymap a = Ls.LayerStack Name Keycode a

class HasKeymap e where
  keymap :: Lens' e (MVar (Keymap ButtonEnv))

--------------------------------------------------------------------------------
-- $keyh

-- | Create a 'KeyHandler' object from a 'Keymap'
initKeymap ::
     Keymap Button            -- ^ A 'Keymap' of all the button configurations
  -> RIO e (Keymap ButtonEnv) -- ^ A 'Keymap' of initialized button environments
initKeymap = itraverseOf (Ls.items . itraversed) (\(_, c) b -> mkButtonEnv b c)

-- | Lookup the 'ButtonEnv' currently mapped to the key press.
lookupKey :: (HasKeymap e, HasLogFunc e)
  => KeyAction               -- ^ The 'KeyAction' to handle
  -> RIO e (Maybe ButtonEnv) -- ^ The resulting action
lookupKey a
  | isRelease a = pure $ Nothing
  | otherwise   = preview (Ls.atKey $ a^.keycode)
                    <$> (view keymap >>= readMVar)

--------------------------------------------------------------------------------
-- $manip
--
-- We provide 2 actions to manipulate the the layer-stack. One that adds a layer
-- to the front, and one that pops the first occurence of a layer. Note that
-- both of these actions can throw errors.

-- | Push a layer to the front of the stack. This throws an error if the
-- provided 'Name' does not correspond to any map.
pushLayer :: (HasKeymap e, HasLogFunc e) => Name -> RIO e ()
pushLayer n =
  view keymap >>= flip modifyMVar (pure . go) >>= \case
    Left e  -> do
      logError $ "Error pushing layer: " <> display n
      throwIO e
    Right _ ->
      logInfo $ "Pushed layer: " <> display n
  where go st = case Ls.pushLayer n st of
          Left e   -> (st, Left e)
          Right km -> (km, Right ())

-- | Pop a layer from the stack. This throws a 'LayerNotOnStackError' if the
-- layer does not exist on the stack, or a 'LayerNotFoundError' if the layer
-- does not exist in the keymap at all.
popLayer :: (HasKeymap e, HasLogFunc e) => Name -> RIO e ()
popLayer n =
  view keymap >>= flip modifyMVar (pure . go) >>= \case
    Left e  -> do
      logError $ "Error popping layer: " <> display n
      throwIO e
    Right _ ->
      logInfo $ "Popped layer: " <> display n
  where go st = case Ls.popLayer n st of
          Left e   -> (st, Left e)
          Right km -> (km, Right ())
