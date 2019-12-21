module KMonad.Daemon.KeyHandler
  ( KeyMapError(..)
  , KeyHandler
  , HasKeyHandler(..)
  , mkKeyHandler
  , handleKey
  , pushLayer
  , removeLayer
  )
where

import KMonad.Prelude

import KMonad.Types.Button
import KMonad.Types.Keyboard.Event
import KMonad.Types.Keymap
import KMonad.Types.Name


import qualified RIO.HashMap           as M
import qualified KMonad.Types.MapStack as S

data KeyMapError
  = LayerNotFoundError Name
  deriving Show

instance Exception KeyMapError

--------------------------------------------------------------------------------

-- | The 'KeyHandler' is responsible for dispatching 'KeyEvent's to various
-- 'Button' 'Action's. Presses and releases are handled asymetrically: A 'Press'
-- is always looked up in the 'KeyMap', a 'MapStack' with a stateful stack. The
-- /topmost/ 'Button' is found and used to register a press event. The 'Button'
-- that initiated the 'Press' is then stored internally a /release-store/.
-- 'Release' events are only dispatched to 'Button's in the internal
-- /release-store/, and once the 'Release' event is dispatched to that 'Button',
-- it is removed from the 'Release' store.
--
-- This means that a 'Button' can never be released without being pressed first
-- (also enforced by the fact that 'Button's ignore any 'Press' after a 'Press'
-- or any 'Release' afer a 'Release'). Additionally, this means that a 'Button'
-- will always be used to process its own 'Release' event: there is no pressing
-- a button is one layer, but then handling its release from another (with
-- another 'Button').
data KeyHandler = KeyHandler
  { -- | The current mapping of 'Keycode' to 'Button'
    _keyMap       :: MVar KeyMap
    -- | 'Button's stored to use as a release
  , _releaseStore :: MVar (M.HashMap Keycode Button)
  }
makeClassy ''KeyHandler

-- | Create a 'KeyHandler' object from a 'KeyMap'
mkKeyHandler :: MonadIO m => KeyMap -> m KeyHandler
mkKeyHandler km = KeyHandler <$> newMVar km <*> newMVar M.empty

-- | Handle a 'KeyAction' using the 'KeyHandler'
handleKey :: (HasKeyAction a, HasKeyHandler e, CanButton e) => a -> RIO e ()
handleKey a
  -- Handle presses by looking them up in the keyMap
  | isPress a = do
      km <- readMVar =<< (view keyMap)
      case S.lookup (a^.keycode) km of
        Nothing -> pure ()
        Just b  -> do
          rs <- view releaseStore
          modifyMVar_ rs $ pure . M.insert (a^.keycode) b
          pressButton b

  -- Handle releases by looking them up in the releaseStore
  | otherwise = do
      rsV <- view releaseStore
      rs  <- takeMVar rsV
      case M.lookup (a^.keycode) rs of
        Nothing -> putMVar rsV rs
        Just b  -> do
          putMVar rsV $ M.delete (a^.keycode) rs
          releaseButton b

-- | Push a LayerId to the front of the 'KeyMap'. This throws an error if the
-- provided 'Name' does not correspond to any map.
pushLayer :: (HasKeyHandler e, HasLogFunc e) => Name -> RIO e ()
pushLayer n = view keyMap >>= flip modifyMVar_ go
  where
    go km = case S.push n km of
      Nothing -> do
        logError $ "Tried to push non-existant layer: " <> fromString (show n)
        throwIO $ LayerNotFoundError n
      Just km' -> do
        logInfo $ "Pushing layer to stack: " <> fromString (show n)
        pure km'

-- | Remove a LayerId from the 'KeyMap'. This throws an error is the provided
-- 'Name' does not currently exist in the layer stack.
removeLayer :: (HasKeyHandler e, HasLogFunc e) => Name -> RIO e ()
removeLayer n = view keyMap >>= flip modifyMVar_ go
  where
    go km = case S.pop n km of
      Nothing -> do
        logError $ "Tried to delete non-existant layer: " <> fromString (show n)
        throwIO $ LayerNotFoundError n
      Just km' -> do
        logInfo $ "Deleting layer from stack: " <> fromString (show n)
        pure km'



-- --------------------------------------------------------------------------------
-- -- Constructing a LayerStack from a set of tokens

-- -- | Turn a nested set of tokens into a layer stack of operations
-- mkLayerStack :: (CanButton m, MonadIO n)
--   => [(Name, [(KeyCode, ButtonToken)])]
--   -> Name
--   -> n (LayerStack m)
-- mkLayerStack ts def = do
--   -- There is probably a much prettier lensy way of doing this
--   bs <- mapM (mapTup (mapM (mapTupB encode))) ts
--   h  <- newMVar . myFromJust "making layer error" . push def . mkMapStack $ bs
--   LayerStack h <$> newMVar (M.empty)
--   where
--     mapTup  f (c, a) = (c,) <$> f a
--     mapTupB f (c, a) = (c,) <$> f c a
